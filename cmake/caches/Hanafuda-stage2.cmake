# This file sets up a CMakeCache for the second stage of a hanafuda distribution
# bootstrap build.

set(PACKAGE_VENDOR "AxioDL" CACHE STRING "")
set(CLANG_VENDOR_UTI "io.github.axiodl.clang" CACHE STRING "")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "Hanafuda" CACHE STRING "")
set(CPACK_PACKAGE_VENDOR "AxioDL" CACHE STRING "")
set(CPACK_PACKAGE_NAME "Hanafuda" CACHE STRING "")

set(LLVM_TARGETS_TO_BUILD PowerPC CACHE STRING "")
set(LLVM_POLLY_BUILD OFF CACHE BOOL "")
set(LLVM_INCLUDE_DOCS OFF CACHE BOOL "")
set(LLVM_INCLUDE_EXAMPLES OFF CACHE BOOL "")

set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "")
if(NOT WIN32)
set(CMAKE_INSTALL_PREFIX "/opt/hanafuda" CACHE PATH "")
set(LLVM_ENABLE_PIC OFF CACHE BOOL "")
set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O3 -gline-tables-only -DNDEBUG" CACHE STRING "")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O3 -gline-tables-only -DNDEBUG" CACHE STRING "")
else()
set(CMAKE_C_FLAGS_RELWITHDEBINFO "/Ox /DNDEBUG" CACHE STRING "")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "/Ox /DNDEBUG" CACHE STRING "")
endif()

# setup toolchain
set(LLVM_INSTALL_TOOLCHAIN_ONLY ON CACHE BOOL "")
set(LLVM_TOOLCHAIN_TOOLS
  llvm-dsymutil
  llvm-cov
  llvm-dwarfdump
  llvm-profdata
  llvm-objdump
  llvm-nm
  llvm-size
  CACHE STRING "")

set(LLVM_DISTRIBUTION_COMPONENTS
  clang
  lld
  clang-format
  clang-headers
  ${LLVM_TOOLCHAIN_TOOLS}
  CACHE STRING "")
