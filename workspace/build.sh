#!/bin/sh

cabal build fficxx-runtime && \
  cabal build fficxx && \
  cabal build stdcxx && \
  cabal exec -- ghc ../Gen.hs -package optparse-applicative && \
  ../Gen gen && \
  cabal build imgui && \
  c++ -c shim.cpp `pkg-config --cflags libimgui` && \
  #cabal exec -- ghc example.hs -framework OpenGL -lglfw -package extra && \
  hsc2hs --cflag=`pkg-config --cflags libimgui` --cc=c++ --ld=c++ StorableInstances.hsc && \
  cabal exec -- ghc StorableInstances.hs custom_rendering.hs shim.o -framework OpenGL -lglfw -package extra && \
  #./example && \
  ./custom_rendering
