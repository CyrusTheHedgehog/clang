//===--- CodeWarriorMangle.cpp - Itanium C++ Name Mangling ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "clang/AST/Mangle.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclOpenMP.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprObjC.h"
#include "clang/AST/TypeLoc.h"
#include "clang/Basic/ABI.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;

namespace {

/// Retrieve the declaration context that should be used when mangling the given
/// declaration.
static const DeclContext *getEffectiveDeclContext(const Decl *D) {
  // The ABI assumes that lambda closure types that occur within
  // default arguments live in the context of the function. However, due to
  // the way in which Clang parses and creates function declarations, this is
  // not the case: the lambda closure type ends up living in the context
  // where the function itself resides, because the function declaration itself
  // had not yet been created. Fix the context here.
  if (const CXXRecordDecl *RD = dyn_cast<CXXRecordDecl>(D)) {
    if (RD->isLambda())
      if (ParmVarDecl *ContextParam
            = dyn_cast_or_null<ParmVarDecl>(RD->getLambdaContextDecl()))
        return ContextParam->getDeclContext();
  }

  // Perform the same check for block literals.
  if (const BlockDecl *BD = dyn_cast<BlockDecl>(D)) {
    if (ParmVarDecl *ContextParam
          = dyn_cast_or_null<ParmVarDecl>(BD->getBlockManglingContextDecl()))
      return ContextParam->getDeclContext();
  }

  const DeclContext *DC = D->getDeclContext();
  if (isa<CapturedDecl>(DC) || isa<OMPDeclareReductionDecl>(DC)) {
    return getEffectiveDeclContext(cast<Decl>(DC));
  }

  if (const auto *VD = dyn_cast<VarDecl>(D))
    if (VD->isExternC())
      return VD->getASTContext().getTranslationUnitDecl();

  if (const auto *FD = dyn_cast<FunctionDecl>(D))
    if (FD->isExternC())
      return FD->getASTContext().getTranslationUnitDecl();

  return DC->getRedeclContext();
}

static const DeclContext *getEffectiveParentContext(const DeclContext *DC) {
  return getEffectiveDeclContext(cast<Decl>(DC));
}

class CodeWarriorMangleContextImpl : public CodeWarriorMangleContext {
public:
  explicit CodeWarriorMangleContextImpl(ASTContext &Context,
                                        DiagnosticsEngine &Diags)
      : CodeWarriorMangleContext(Context, Diags) {}

  /// @name Mangler Entry Points
  /// @{

  bool shouldMangleCXXName(const NamedDecl *D) override;
  bool shouldMangleStringLiteral(const StringLiteral *) override {
    return false;
  }
  void mangleCXXName(const NamedDecl *D, raw_ostream &) override;
  void mangleThunk(const CXXMethodDecl *MD, const ThunkInfo &Thunk,
                   raw_ostream &) override;
  void mangleCXXDtorThunk(const CXXDestructorDecl *DD, CXXDtorType Type,
                          const ThisAdjustment &ThisAdjustment,
                          raw_ostream &) override;
  void mangleReferenceTemporary(const VarDecl *D, unsigned ManglingNumber,
                                raw_ostream &) override;
  void mangleCXXRTTI(QualType T, raw_ostream &) override;
  void mangleCXXRTTIName(QualType T, raw_ostream &) override;
  void mangleTypeName(QualType T, raw_ostream &) override;
  void mangleCXXCtor(const CXXConstructorDecl *D, CXXCtorType Type,
                     raw_ostream &) override;
  void mangleCXXDtor(const CXXDestructorDecl *D, CXXDtorType Type,
                     raw_ostream &) override;

  void mangleStaticGuardVariable(const VarDecl *D, raw_ostream &) override;
  void mangleDynamicInitializer(const VarDecl *D, raw_ostream &Out) override;
  void mangleDynamicAtExitDestructor(const VarDecl *D,
                                     raw_ostream &Out) override;
  void mangleSEHFilterExpression(const NamedDecl *EnclosingDecl,
                                 raw_ostream &Out) override;
  void mangleSEHFinallyBlock(const NamedDecl *EnclosingDecl,
                             raw_ostream &Out) override;

  void mangleStringLiteral(const StringLiteral *, raw_ostream &) override;

  void mangleCXXVTable(const CXXRecordDecl *RD, raw_ostream &) override;
  void mangleCXXVTT(const CXXRecordDecl *RD, raw_ostream &) override;
  void mangleCXXCtorVTable(const CXXRecordDecl *RD, int64_t Offset,
                           const CXXRecordDecl *Type,
                           raw_ostream &) override;
  void mangleItaniumThreadLocalInit(const VarDecl *D,
                                    raw_ostream &) override;
  void mangleItaniumThreadLocalWrapper(const VarDecl *D,
                                       raw_ostream &) override;

  void mangleCXXCtorComdat(const CXXConstructorDecl *D,
                           raw_ostream &) override;
  void mangleCXXDtorComdat(const CXXDestructorDecl *D,
                           raw_ostream &) override;

  /// @}
};

}

bool CodeWarriorMangleContextImpl::shouldMangleCXXName(const NamedDecl *D) {
  const FunctionDecl *FD = dyn_cast<FunctionDecl>(D);
  if (FD) {
    // Pragma patch function declarations as extern "C" aren't mangled
    if (FD->getDeclContext()->isPragmaPatch() &&
        FD->getDeclContext()->isExternCContext())
      return false;

    LanguageLinkage L = FD->getLanguageLinkage();
    // Overloadable functions need mangling.
    if (FD->hasAttr<OverloadableAttr>())
      return true;

    // "main" is not mangled.
    if (FD->isMain())
      return false;

    // C++ functions and those whose names are not a simple identifier need
    // mangling.
    if (!FD->getDeclName().isIdentifier() || L == CXXLanguageLinkage)
      return true;

    // C functions are not mangled.
    if (L == CLanguageLinkage)
      return false;
  }

  // Otherwise, no mangling is done outside C++ mode.
  if (!getASTContext().getLangOpts().CPlusPlus)
    return false;

  const VarDecl *VD = dyn_cast<VarDecl>(D);
  if (VD && !isa<DecompositionDecl>(D)) {
    // Pragma patch variable declarations as extern "C" aren't mangled
    if (VD->getDeclContext()->isPragmaPatch() &&
        VD->getDeclContext()->isExternCContext())
      return false;

    // C variables are not mangled.
    if (VD->isExternC())
      return false;

    // Variables at global scope with non-internal linkage are not mangled
    const DeclContext *DC = getEffectiveDeclContext(D);
    // Check for extern variable declared locally.
    if ((DC->isFunctionOrMethod() && D->hasLinkage()) ||
        isa<PragmaPatchDecl>(DC))
      while (!DC->isNamespace() && !DC->isTranslationUnit())
        DC = getEffectiveParentContext(DC);
    if (DC->isTranslationUnit() && D->getFormalLinkage() != InternalLinkage &&
        !isa<VarTemplateSpecializationDecl>(D))
      return false;
  }

  return true;
}

static bool PrintType(QualType T, const ASTContext &Ctx,
                      raw_ostream &Out);

static void MangleTemplateSpecialization(const TemplateArgumentList &List,
                                         const ASTContext &Ctx,
                                         raw_ostream &Out) {
  Out << '<';
  bool NeedsComma = false;
  for (const TemplateArgument& Arg : List.asArray()) {
    switch (Arg.getKind()) {
    case TemplateArgument::Type:
      if (NeedsComma)
        Out << ',';
      PrintType(Arg.getAsType(), Ctx, Out);
      NeedsComma = true;
      break;
    case TemplateArgument::Integral:
      if (NeedsComma)
        Out << ',';
      Arg.getAsIntegral().print(Out, true);
      NeedsComma = true;
      break;
    default: break;
    }
  }
  Out << '>';
}

static void MangleTemplateSpecialization(
    const DependentFunctionTemplateSpecializationInfo &List,
    const ASTContext &Ctx, raw_ostream &Out) {
  Out << '<';
  bool NeedsComma = false;
  const TemplateArgumentLoc *Args = List.getTemplateArgs();
  for (unsigned i = 0; i < List.getNumTemplateArgs(); ++i) {
    const TemplateArgument &Arg = Args[i].getArgument();
    switch (Arg.getKind()) {
    case TemplateArgument::Type:
      if (NeedsComma)
        Out << ',';
      PrintType(Arg.getAsType(), Ctx, Out);
      NeedsComma = true;
      break;
    case TemplateArgument::Integral:
      if (NeedsComma)
        Out << ',';
      Arg.getAsIntegral().print(Out, true);
      NeedsComma = true;
      break;
    default: break;
    }
  }
  Out << '>';
}

static void MangleClassTemplateSpecialization(const Decl *Decl,
                                              const ASTContext &Ctx,
                                              raw_ostream &Out) {
  if (const ClassTemplateSpecializationDecl *TemplateSpec =
      dyn_cast<ClassTemplateSpecializationDecl>(Decl)) {
    const TemplateArgumentList &List = TemplateSpec->getTemplateInstantiationArgs();
    MangleTemplateSpecialization(List, Ctx, Out);
  }
}

static void PrintNamedDecl(const NamedDecl *ND, const ASTContext &Ctx,
                           raw_ostream &Out) {
  std::string Str;
  llvm::raw_string_ostream Name(Str);
  Name << ND->getName();
  MangleClassTemplateSpecialization(ND, Ctx, Name);
  auto &NameStr = Name.str();
  Out << NameStr.size();
  Out << NameStr;
}

static void RecursiveDenest(const DeclContext *DCtx,
                            unsigned Count,
                            const ASTContext &Ctx,
                            raw_ostream &Out) {
  const NamedDecl *Named = dyn_cast<NamedDecl>(DCtx);
  if (!Named)
    return;
  const DeclContext *Prefix = DCtx->getParent();
  if (Prefix && isa<NamedDecl>(Prefix))
    RecursiveDenest(Prefix, Count + 1, Ctx, Out);
  else if (Count > 1)
    Out << 'Q' << Count;

  PrintNamedDecl(Named, Ctx, Out);
}

static bool PrintType(QualType T, const ASTContext &Ctx,
                      raw_ostream &Out) {
  if (const ReferenceType *Ref = T.getTypePtr()->getAs<ReferenceType>()) {
    Out << 'R';
    if (Ref->getPointeeType().isConstant(Ctx))
      Out << 'C';
    return PrintType(Ref->getPointeeType(), Ctx, Out);

  } else if (const PointerType *Ptr = T.getTypePtr()->getAs<PointerType>()) {
    Out << 'P';
    if (Ptr->getPointeeType().isConstant(Ctx))
      Out << 'C';
    return PrintType(Ptr->getPointeeType(), Ctx, Out);

  } else if (const TagType *Tag = T.getTypePtr()->getAs<TagType>()) {
    const TagDecl *TD = Tag->getDecl();
    RecursiveDenest(getEffectiveDeclContext(TD), 2, Ctx, Out);
    PrintNamedDecl(TD, Ctx, Out);
    return true;

  } else if (const BuiltinType *Builtin = T.getTypePtr()->getAs<BuiltinType>()) {
    switch (Builtin->getKind()) {
    case BuiltinType::Void:
      Out << 'v';
      return true;
    case BuiltinType::Bool:
      Out << 'b';
      return true;
    case BuiltinType::Char_U:
    case BuiltinType::UChar:
      Out << "Uc";
      return true;
    case BuiltinType::WChar_U:
    case BuiltinType::Char16:
      Out << "Uw";
      return true;
    case BuiltinType::UShort:
      Out << "Us";
      return true;
    case BuiltinType::UInt:
      Out << "Ui";
      return true;
    case BuiltinType::ULong:
      Out << "Ul";
      return true;
    case BuiltinType::ULongLong:
      Out << "Uq";
      return true;
    case BuiltinType::Char_S:
    case BuiltinType::SChar:
      Out << 'c';
      return true;
    case BuiltinType::WChar_S:
      Out << 'w';
      return true;
    case BuiltinType::Short:
      Out << 's';
      return true;
    case BuiltinType::Int:
      Out << 'i';
      return true;
    case BuiltinType::Long:
      Out << 'l';
      return true;
    case BuiltinType::LongLong:
      Out << 'q';
      return true;
    case BuiltinType::Float:
      Out << 'f';
      return true;
    case BuiltinType::Double:
      Out << 'd';
      return true;
    default: break;
    }
  }
  return false;
}

/// Mangles the name of the declaration D and emits that name to the given
/// output stream.
///
/// If the declaration D requires a mangled name, this routine will emit that
/// mangled name to \p os and return true. Otherwise, \p os will be unchanged
/// and this routine will return false. In this case, the caller should just
/// emit the identifier of the declaration (\c D->getIdentifier()) as its
/// name.
void CodeWarriorMangleContextImpl::mangleCXXName(const NamedDecl *D,
                                                 raw_ostream &Out) {
  assert((isa<FunctionDecl>(D) || isa<VarDecl>(D)) &&
          "Invalid mangleName() call, argument is not a variable or function!");
  assert(!isa<CXXConstructorDecl>(D) && !isa<CXXDestructorDecl>(D) &&
         "Invalid mangleName() call on 'structor decl!");

  PrettyStackTraceDecl CrashInfo(D, SourceLocation(),
                                 getASTContext().getSourceManager(),
                                 "Mangling declaration");

  if (const CXXMethodDecl *MD = dyn_cast<CXXMethodDecl>(D)) {
    if (isa<CXXConstructorDecl>(D))
      Out << "__ct";
    else if (isa<CXXDestructorDecl>(D))
      Out << "__dt";
    else
      MD->getNameInfo().printName(Out);
    if (const TemplateArgumentList *TArgs = MD->getTemplateSpecializationArgs())
      MangleTemplateSpecialization(*TArgs, getASTContext(), Out);
    else if (DependentFunctionTemplateSpecializationInfo *DepArgs =
             MD->getDependentSpecializationInfo())
      MangleTemplateSpecialization(*DepArgs, getASTContext(), Out);
    Out << "__";
    RecursiveDenest(getEffectiveDeclContext(MD), 1,
                    getASTContext(), Out);
    if (MD->isConst())
      Out << 'C';
    Out << 'F';

    if (MD->param_empty())
      Out << 'v';
    else
      for (const ParmVarDecl *Param : MD->parameters())
        PrintType(Param->getType(), getASTContext(), Out);

  } else if (const FunctionDecl *FD = dyn_cast<FunctionDecl>(D)) {
    FD->getNameInfo().printName(Out);
    if (const TemplateArgumentList *TArgs = FD->getTemplateSpecializationArgs())
      MangleTemplateSpecialization(*TArgs, getASTContext(), Out);
    else if (DependentFunctionTemplateSpecializationInfo *DepArgs =
             FD->getDependentSpecializationInfo())
      MangleTemplateSpecialization(*DepArgs, getASTContext(), Out);
    Out << "__";
    RecursiveDenest(getEffectiveDeclContext(FD), 1,
                    getASTContext(), Out);
    Out << 'F';

    if (FD->param_empty())
      Out << 'v';
    else
      for (const ParmVarDecl *Param : FD->parameters())
        PrintType(Param->getType(), getASTContext(), Out);

  } else if (const VarDecl *VD = dyn_cast<VarDecl>(D)) {
    Out << VD->getName();
    Out << "__";
    RecursiveDenest(getEffectiveDeclContext(VD), 1,
                    getASTContext(), Out);

  }
}

void CodeWarriorMangleContextImpl::mangleCXXCtor(const CXXConstructorDecl *D,
                                                 CXXCtorType Type,
                                                 raw_ostream &Out) {

}

void CodeWarriorMangleContextImpl::mangleCXXDtor(const CXXDestructorDecl *D,
                                                 CXXDtorType Type,
                                                 raw_ostream &Out) {

}

void CodeWarriorMangleContextImpl::mangleThunk(const CXXMethodDecl *MD,
                                               const ThunkInfo &Thunk,
                                               raw_ostream &Out) {
}

void CodeWarriorMangleContextImpl::mangleCXXDtorThunk(
    const CXXDestructorDecl *DD, CXXDtorType Type,
    const ThisAdjustment &ThisAdjustment, raw_ostream &Out) {
}

/// Returns the mangled name for a guard variable for the passed in VarDecl.
void CodeWarriorMangleContextImpl::mangleStaticGuardVariable(const VarDecl *D,
                                                         raw_ostream &Out) {
}

void CodeWarriorMangleContextImpl::mangleDynamicInitializer(const VarDecl *MD,
                                                            raw_ostream &Out) {
  // These symbols are internal in the Itanium ABI, so the names don't matter.
  // Clang has traditionally used this symbol and allowed LLVM to adjust it to
  // avoid duplicate symbols.
  Out << "__cxx_global_var_init";
}

void CodeWarriorMangleContextImpl::mangleDynamicAtExitDestructor(const VarDecl *D,
                                                                 raw_ostream &Out) {
  // Prefix the mangling of D with __dtor_.
  Out << "__dtor_";
  if (shouldMangleDeclName(D)) {
  } else
    Out << D->getName();
}

void CodeWarriorMangleContextImpl::mangleSEHFilterExpression(
    const NamedDecl *EnclosingDecl, raw_ostream &Out) {
  Out << "__filt_";
  if (shouldMangleDeclName(EnclosingDecl)) {
  } else
    Out << EnclosingDecl->getName();
}

void CodeWarriorMangleContextImpl::mangleSEHFinallyBlock(
    const NamedDecl *EnclosingDecl, raw_ostream &Out) {
  Out << "__fin_";
  if (shouldMangleDeclName(EnclosingDecl)) {
  } else
    Out << EnclosingDecl->getName();
}

void CodeWarriorMangleContextImpl::mangleReferenceTemporary(const VarDecl *D,
                                                            unsigned ManglingNumber,
                                                            raw_ostream &Out) {
}

void CodeWarriorMangleContextImpl::mangleCXXRTTI(QualType Ty, raw_ostream &Out) {
  // <special-name> ::= TI <type>  # typeinfo structure
  assert(!Ty.hasQualifiers() && "RTTI info cannot have top-level qualifiers");
  Out << "_ZTI";
  PrintType(Ty, getASTContext(), Out);
}

void CodeWarriorMangleContextImpl::mangleCXXRTTIName(QualType Ty,
                                                     raw_ostream &Out) {
  // <special-name> ::= TS <type>  # typeinfo name (null terminated byte string)
  Out << "_ZTS";
  PrintType(Ty, getASTContext(), Out);
}

void CodeWarriorMangleContextImpl::mangleTypeName(QualType Ty, raw_ostream &Out) {
  mangleCXXRTTIName(Ty, Out);
}

void CodeWarriorMangleContextImpl::mangleStringLiteral(
    const StringLiteral *, raw_ostream &) {
  llvm_unreachable("Can't mangle string literals");
}

void CodeWarriorMangleContextImpl::mangleCXXVTable(
    const CXXRecordDecl *RD, raw_ostream &) {

}

void CodeWarriorMangleContextImpl::mangleCXXVTT(
    const CXXRecordDecl *RD, raw_ostream &) {

}

void CodeWarriorMangleContextImpl::mangleCXXCtorVTable(
    const CXXRecordDecl *RD, int64_t Offset,
    const CXXRecordDecl *Type, raw_ostream &) {

}

void CodeWarriorMangleContextImpl::mangleItaniumThreadLocalInit(
    const VarDecl *D, raw_ostream &) {

}

void CodeWarriorMangleContextImpl::mangleItaniumThreadLocalWrapper(
    const VarDecl *D, raw_ostream &) {

}

void CodeWarriorMangleContextImpl::mangleCXXCtorComdat(
    const CXXConstructorDecl *D, raw_ostream &) {

}

void CodeWarriorMangleContextImpl::mangleCXXDtorComdat(
    const CXXDestructorDecl *D, raw_ostream &) {

}

CodeWarriorMangleContext *
CodeWarriorMangleContext::create(ASTContext &Context, DiagnosticsEngine &Diags) {
  return new CodeWarriorMangleContextImpl(Context, Diags);
}
