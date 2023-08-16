#!/bin/sh

cabal build fficxx-runtime && \
  cabal build fficxx && \
  cabal build stdcxx && \
  cabal exec -- ghc ../imgui-gen/Gen.hs -package optparse-applicative && \
  ../imgui-gen/Gen gen --template=../imgui-gen/template && \
  cabal build imgui && \
  cabal exec -- ghc ../implot-gen/Gen.hs -package optparse-applicative && \
  ../implot-gen/Gen gen --template=../implot-gen/template && \
  cabal build implot && \
  c++ -c shim.cpp `pkg-config --cflags libimgui` && \
  cabal exec -- ghc demo-imgui-builtin.hs -framework OpenGL -lglfw -package extra && \
  cabal exec -- ghc demo-implot-builtin.hs -framework OpenGL -lglfw -package extra && \
  hsc2hs --cflag=`pkg-config --cflags libimgui` --cc=c++ --ld=c++ StorableInstances.hsc && \
  cabal exec -- ghc StorableInstances.hs custom_rendering.hs shim.o -framework OpenGL -lglfw -package extra && \
  ./demo-imgui-builtin && \
  ./custom_rendering && \
  ./demo-implot-builtin
