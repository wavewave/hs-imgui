#!/bin/sh

cabal build fficxx-runtime && \
  cabal build fficxx && \
  cabal build stdcxx && \
  cabal exec -- ghc ../Gen.hs -package optparse-applicative && \
  ../Gen gen && \
  cabal build imgui && \
  cabal exec -- ghc test.hs -framework OpenGL -lglfw -package extra && \
  ./test
