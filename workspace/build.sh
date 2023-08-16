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
  cabal exec -- ghc demo-imgui-builtin.hs -framework OpenGL -lglfw -package extra && \
  cabal exec -- ghc demo-implot-builtin.hs -framework OpenGL -lglfw -package extra && \
  ./demo-imgui-builtin && \
  ./demo-implot-builtin && \
  cabal run draw-demo && \
  cabal run plot-demo
