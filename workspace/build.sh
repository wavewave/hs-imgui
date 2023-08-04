#!/bin/sh

cabal build fficxx-runtime
cabal build fficxx
cabal build stdcxx

rm ../Gen.o
cabal exec -- ghc ../Gen.hs
../Gen gen

cabal build imgui

