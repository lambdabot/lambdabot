#!/bin/bash

# Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
# GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)


if [ ! -e "lambdabot.cabal" ] ; then
  echo "You must run this script from the root directory of lambdabot package."
  echo "Change your current path to the directory containing lambdabot.cabal"
  echo "file and type:"
  echo ""
  echo "./scripts/ghci.sh"
  echo ""
  echo "Make also sure you've done a full compiled build of lambdabot recently."
  exit
fi

echo "Make sure you've done a full compiled build of lambdabot recently."
echo ""

# find possible .o files
if [ -e "dist/build/lambdabot/lambdabot-tmp/" ] ; then
    Odir=dist/build/lambdabot/lambdabot-tmp/
else
    Odir=.
fi

# run ghci with the right command line flags to launch lambdabot
ghci -cpp -Wall -I. -isrc -idist/build/autogen/ -fno-warn-incomplete-patterns -fno-warn-missing-methods -fno-warn-orphans -DGHCi -hidir $Odir -odir $Odir $* -XCPP -XDeriveDataTypeable -XExistentialQuantification -XFlexibleContexts -XFlexibleInstances -XImplicitParams -XMultiParamTypeClasses -XNoMonomorphismRestriction -XPatternGuards -XRank2Types -XScopedTypeVariables -XStandaloneDeriving -XTemplateHaskell -XTypeOperators -XTypeSynonymInstances -XUndecidableInstances -XViewPatterns
