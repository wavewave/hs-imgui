#!/bin/sh

cabal build fficxx-runtime
cabal build fficxx
cabal build stdcxx

rm ../Gen.o
cabal exec -- ghc ../Gen.hs -package optparse-applicative
../Gen gen

cabal build imgui


cabal exec -- ghc test.hs -limgui
