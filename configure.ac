
# sanity
AC_INIT(Main.hs)

# this is arbitrary
AC_PREREQ([2.52])

#
# Looking for HC, stolen from $fptools
#
AC_ARG_WITH(hc,
[AC_HELP_STRING(
 	[--with-hc=ARG],
	[Use ARG as the path to a Haskell compiler [default=autodetect]])
],
[ WithGhc="$withval"
  if test "$WithGhc" = "nhc98"; then
	AC_DEFINE(__NHC__, [], [Defined if compiling with nhc98])
	SYMS="$SYMS -D__NHC__"
  fi
],
[
  # else check if --with-ghc was used
  AC_ARG_WITH(ghc,
  [AC_HELP_STRING(
   	[--with-ghc=ARG],
	[Use ARG as the path to a Haskell compiler [default=autodetect]])
  ],
  [ WithGhc="$withval"
    if test "$WithGhc" = "nhc98"; then
	  AC_DEFINE(__NHC__, [], [Defined if compiling with nhc98])
	  SYMS="$SYMS -D__NHC__"
    fi
    if test ! -x "$WithGhc" ; then
	    AC_MSG_ERROR([Cannot find a working GHC at: $WithGhc])
    fi
  ],
  [
     # else autodetect
     if test "$GHC" = ""; then
       AC_CHECK_PROGS(GHC,ghc)
     fi
     if test "$GHC" = ""; then
       AC_CHECK_PROGS(GHC,nhc98)
     fi
     if test "$GHC" = "nhc98"; then
       AC_DEFINE(__NHC__, [], [Defined if compiling with nhc98])
       SYMS="$SYMS -D__NHC__"
     fi
     WithGhc=$GHC
   ])
])
AC_SUBST(WithGhc)

# check ghc version here
if test -n "$WithGhc" ; then
    AC_MSG_CHECKING([for ghc version])
    GHC_VERSION=`$WithGhc --numeric-version`
    AC_MSG_RESULT([$GHC_VERSION])
fi
AC_SUBST(GHC_VERSION)

# Work out value of __GLASGOW_HASKELL__
if test -n "$WithGhc" ; then
 AC_MSG_CHECKING([for value of __GLASGOW_HASKELL__])
 echo "main = print __GLASGOW_HASKELL__" > t.hs
 GLASGOW_HASKELL=`echo 'main' | "$WithGhc" -ignore-dot-ghci --interactive -v0 -cpp t.hs`
 rm t.hs
 AC_MSG_RESULT([$GLASGOW_HASKELL])
fi
AC_SUBST(GLASGOW_HASKELL)

# check ghc library path
if test -n "$WithGhc" ; then
    AC_MSG_CHECKING([for ghc library path])
    GHC_LIB_PATH=`$WithGhc --print-libdir`
    AC_MSG_RESULT([$GHC_LIB_PATH])
fi
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

AC_CHECK_PROG(HaddockCmd,haddock,haddock)
AC_CHECK_PROG(AspellCmd,aspell,aspell)

if test -n "$AspellCmd" ; then
    HAVE_ASPELL=1
else
    HAVE_ASPELL=0
fi
AC_SUBST(HAVE_ASPELL)

AC_SUBST(SYMS)

AC_CONFIG_FILES([config.mk])
AC_CONFIG_FILES([config.h])
AC_OUTPUT
