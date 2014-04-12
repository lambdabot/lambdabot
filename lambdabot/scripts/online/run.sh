#!/bin/sh
cat lambdabot.log >> lambdabot.log.old
LC_ALL=en_US.UTF-8 PATH=$HOME/.cabal/bin:$HOME/bin:$PATH \
  exec lambdabot \
  --trust=base \
  --trust=array \
  --trust=bytestring \
  --trust=containers \
  --trust=random \
  --trust=parallel \
  --trust=hashable \
  --trust=lens \
  --trust=semigroupoids \
  --trust=text \
  --trust=lambdabot-trusted \
  -X BangPatterns \
  -X ConstrainedClassMethods \
  -X ConstraintKinds \
  -X DataKinds \
  -X DeriveDataTypeable \
  -X DeriveFoldable \
  -X DeriveFunctor \
  -X DeriveGeneric \
  -X DeriveTraversable \
  -X EmptyCase \
  -X EmptyDataDecls \
  -X ExistentialQuantification \
  -X ExtendedDefaultRules \
  -X FlexibleContexts \
  -X FlexibleInstances \
  -X FunctionalDependencies \
  -X GADTs \
  -X ImplicitParams \
  -X ImplicitPrelude \
  -X KindSignatures \
  -X LambdaCase \
  -X LiberalTypeSynonyms \
  -X MagicHash \
  -X MultiParamTypeClasses \
  -X NoMonomorphismRestriction \
  -X PackageImports \
  -X ParallelListComp \
  -X PartialTypeSignatures \
  -X PatternGuards \
  -X PolyKinds \
  -X PolymorphicComponents \
  -X PostfixOperators \
  -X RankNTypes \
  -X ScopedTypeVariables \
  -X StandaloneDeriving \
  -X TupleSections \
  -X TypeFamilies \
  -X TypeOperators \
  -X TypeSynonymInstances \
  -X UndecidableInstances \
  -X UnicodeSyntax \
  -X ViewPatterns \
  -e 'rc online.rc' \
  > lambdabot.log 2>&1
# -X UnboxedTuples # cf. https://gitlab.haskell.org/ghc/ghc/-/issues/15454
# --trust=stm
# --trust=unordered-containers
# --trust=exceptions
# --trust=prelude-extras
# --trust=base-orphans
# --trust=bifunctors
# --trust=profunctors
# --trust=reflection
# --trust=adjunctions
# --trust=kan-extensions
# --trust=tagged
# --trust=comonad
