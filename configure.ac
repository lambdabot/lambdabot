
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

# Use GNU m4, which is called gm4 on *BSD
if gm4 --version > /dev/null 2>&1 ; then
        AC_PATH_PROG(WithM4,gm4)
else
        AC_PATH_PROG(WithM4,m4)
fi

# need to recompile this if configure is rerun
touch Plugin/Version.hs

AC_SUBST(SYMS)

AC_CONFIG_FILES([config.h testsuite/logpp])
AC_CONFIG_COMMANDS([default], [chmod 700 testsuite/logpp] )
AC_OUTPUT
