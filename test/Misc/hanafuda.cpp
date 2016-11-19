// RUN: %clang --driver-mode=hanafuda++ -fsyntax-only -Xpreprocessor -ast-dump %s 2>&1 | FileCheck %s

extern "C" {
#pragma patch_dol(int main(), int patch_main())
}

// CHECK:      |-LinkageSpecDecl {{.*}} <{{.*}}:3:1, line:5:1> line:3:8 C
// CHECK-NEXT: | `-PragmaPatchDecl {{.*}} <line:4:9> col:9
// CHECK-NEXT: |   |-FunctionDecl {{.*}} <col:19, col:28> col:23 main 'int (void)'
// CHECK-NEXT: |   `-FunctionDecl {{.*}} <col:31, col:46> col:35 patch_main 'int (void)'

extern "C" void InitMetroTRK();
volatile int SmallOne = 1;
volatile const int SmallTwo = 2;
extern "C" int patch_main(int argc, char** argv) {
  InitMetroTRK();
  return SmallOne + SmallTwo;
}

template <class T>
class TLockedToken {

};

class CCharacterFactory {
public:
  int PatchCreateCharacter(int, bool, const TLockedToken<CCharacterFactory>&, int) const {
    return 32;
  }
};
#pragma patch_dol(int CCharacterFactory::CreateCharacter(int, bool, const TLockedToken<CCharacterFactory>&, int) const, \
                  int CCharacterFactory::PatchCreateCharacter(int, bool, const TLockedToken<CCharacterFactory>&, int) const)

// CHECK:      |-PragmaPatchDecl {{.*}} <line:31:9> col:9
// CHECK-NEXT: | |-CXXMethodDecl {{.*}} parent {{.*}} <col:19, col:114> col:42 CreateCharacter 'int (int, _Bool, const TLockedToken<class CCharacterFactory> &, int) const'
// CHECK-NEXT: | | |-ParmVarDecl {{.*}} <col:58> col:61 'int'
// CHECK-NEXT: | | |-ParmVarDecl {{.*}} <col:63> col:67 '_Bool'
// CHECK-NEXT: | | |-ParmVarDecl {{.*}} <col:69, col:106> col:107 'const TLockedToken<class CCharacterFactory> &'
// CHECK-NEXT: | | `-ParmVarDecl {{.*}} <col:109> col:112 'int'
// CHECK-NEXT: | `-CXXMethodDecl {{.*}} parent {{.*}} <line:32:19, col:119> col:42 PatchCreateCharacter 'int (int, _Bool, const TLockedToken<class CCharacterFactory> &, int) const'
// CHECK-NEXT: |   |-ParmVarDecl {{.*}} <col:63> col:66 'int'
// CHECK-NEXT: |   |-ParmVarDecl {{.*}} <col:68> col:72 '_Bool'
// CHECK-NEXT: |   |-ParmVarDecl {{.*}} <col:74, col:111> col:112 'const TLockedToken<class CCharacterFactory> &'
// CHECK-NEXT: |   `-ParmVarDecl {{.*}} <col:114> col:117 'int'

namespace BunchaStuff {

template <class T>
struct MyStruct {
template <class U, int ADD>
T MyMethod(T theInt, U otherInt) const {
  return theInt + otherInt + ADD;
}
};

template struct MyStruct<int>;

int MyGlobal = 20;

template <class U>
int MyFunction(const MyStruct<U>& str) {
  CCharacterFactory fac;
  TLockedToken<CCharacterFactory> Tok;
  fac.PatchCreateCharacter(0, false, Tok, 0);
  return str.template MyMethod<unsigned, 5>(30, 30);
}

template int MyFunction<int>(const MyStruct<int>& str);

}

class CVParamTransfer {
};

#pragma patch_dol(int CVParamTransfer::Null(), template<> int BunchaStuff::MyFunction<int>(const BunchaStuff::MyStruct<int>&))

// CHECK:      `-PragmaPatchDecl {{.*}} <line:75:9> col:9
// CHECK-NEXT:   |-CXXMethodDecl {{.*}} parent {{.*}} <col:19, col:45> col:40 Null 'int (void)'
// CHECK-NEXT:   `-FunctionDecl {{.*}} parent {{.*}} <col:48, col:125> col:76 MyFunction 'int (const BunchaStuff::MyStruct<int> &)'
// CHECK-NEXT:     `-ParmVarDecl {{.*}} <col:92, col:124> col:125 'const BunchaStuff::MyStruct<int> &'
