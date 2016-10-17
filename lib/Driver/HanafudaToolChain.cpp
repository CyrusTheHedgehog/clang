//===--- HanafudaToolChain.cpp - Hanafuda Implementation ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "ToolChains.h"
#include "Tools.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/Version.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/DriverDiagnostic.h"
#include "clang/Driver/Options.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Process.h"
#include "InputInfo.h"
#include <cstdio>

namespace clang {
namespace driver {

namespace tools {
namespace hanafuda {
class LLVM_LIBRARY_VISIBILITY Linker : public Tool {
public:
  Linker(const ToolChain &TC) : Tool("hanafuda::Linker", "linker", TC, RF_Full) {}

  bool hasIntegratedCPP() const override { return false; }
  bool isLinkJob() const override { return true; }

  void ConstructJob(Compilation &C, const JobAction &JA,
                    const InputInfo &Output,
                    const InputInfoList &Inputs,
                    const llvm::opt::ArgList &TCArgs,
                    const char *LinkingOutput) const override
  {
    ArgStringList CmdArgs;

    TCArgs.AddAllArgs(CmdArgs, {options::OPT_hanafuda_base_dol, options::OPT_hanafuda_base_rel,
                                options::OPT_hanafuda_text_section, options::OPT_hanafuda_data_section,
                                options::OPT_hanafuda_dol_symbol_list, options::OPT_hanafuda_rel_symbol_list});

    auto Exec = getToolChain().GetProgramPath("lld-hanafuda");
    C.addCommand(llvm::make_unique<Command>(JA, *this, Exec.c_str(), CmdArgs, Inputs));
  }
};
}
}

namespace toolchains {

HanafudaToolChain::HanafudaToolChain(const Driver &D, const llvm::Triple &Triple,
                                     const llvm::opt::ArgList &Args)
  : ToolChain(D, Triple, Args)
{
  getProgramPaths().push_back(getDriver().getInstalledDir());
}

Tool *HanafudaToolChain::buildLinker() const { return new tools::hanafuda::Linker(*this); }

} // end namespace toolchains
} // end namespace driver
} // end namespace clang
