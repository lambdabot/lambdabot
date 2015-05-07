#! /bin/sh
curl -L https://www.haskell.org/ghc/docs/latest/html/libraries/doc-index-All.html | runhaskell GenHaddock.hs | gzip -9 > ../State/haddock
