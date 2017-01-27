// Check passing PowerPC EABI small-data threshold to the backend.

// RUN: %clang -target powerpc-unknown-unknown-eabi -G 0 %s -### -o %t.o 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK-SDATA0 %s
// RUN: %clang -target powerpc-unknown-unknown-eabi -G 8 %s -### -o %t.o 2>&1 \
// RUN:   | FileCheck -check-prefix=CHECK-SDATA8 %s

// CHECK-SDATA0: "-mllvm" "-ppc-ssection-threshold=0"
// CHECK-SDATA8: "-mllvm" "-ppc-ssection-threshold=8"
