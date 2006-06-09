
# sanity
AC_INIT(Main.hs)

# this is arbitrary
AC_PREREQ([2.52])

# check ghc version here
AC_MSG_CHECKING([for ghc version])
GHC_VERSION=`ghc --numeric-version`
AC_MSG_RESULT([$GHC_VERSION])
AC_SUBST(GHC_VERSION)

# check ghc library path
AC_MSG_CHECKING([for ghc library path])
GHC_LIB_PATH=`ghc --print-libdir`
AC_MSG_RESULT([$GHC_LIB_PATH])
AC_SUBST(GHC_LIB_PATH)

PLATFORM=`uname -ms`
AC_SUBST(PLATFORM)

# extracting the cpu details (linux only)
CPU=`grep 'model name' /proc/cpuinfo 2> /dev/null | sed 's/.*: //' | sed 's/.* //;1q' || true`
AC_SUBST(CPU)

REPO_PATH=`sed -n 1p _darcs/prefs/repos`
AC_SUBST(REPO_PATH)

PATCH_COUNT=`darcs changes --xml-output 2> /dev/null | sed -n '/TAG/q;/^<\/patch/p' | wc -l | sed 's/ *//g'`
AC_SUBST(PATCH_COUNT)

# need to recompile this if configure is rerun
touch Plugin/Version.hs

AC_SUBST(SYMS)

AC_CONFIG_FILES([config.h])
AC_OUTPUT
