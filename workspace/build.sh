#!/bin/sh

rm ../Gen.o
ghc ../Gen.hs
../Gen gen
cabal build imgui

