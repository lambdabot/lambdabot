
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
 GLASGOW_HASKELL=`echo 'main' | "$WithGhc" --interactive -v0 -cpp t.hs`
 rm t.hs
 AC_MSG_RESULT([$GLASGOW_HASKELL])
fi
AC_SUBST(GLASGOW_HASKELL)

BUILD_DATE=`date`
AC_SUBST(BUILD_DATE)

PLATFORM=`uname -ms`
AC_SUBST(PLATFORM)

REPO_PATH=`sed -n 1p ../_darcs/prefs/repos`
AC_SUBST(REPO_PATH)

AC_CHECK_PROG(HaddockCmd,haddock,haddock)

AC_SUBST(SYMS)

AC_CONFIG_FILES([config.mk])
AC_CONFIG_FILES([config.h])
AC_OUTPUT
