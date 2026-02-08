#!/bin/sh

ghc ../imgui-gen/Gen.hs -package optparse-applicative && \
../imgui-gen/Gen gen --template=../imgui-gen/template && \
ghc ../implot-gen/Gen.hs -package optparse-applicative && \
../implot-gen/Gen gen --template=../implot-gen/template && \
ghc ../implot3d-gen/Gen.hs -package optparse-applicative && \
../implot3d-gen/Gen gen --template=../implot-gen/template && \
cabal build imgui && \
cabal build implot && \
cabal run draw-demo && \
cabal run plot-demo

#cabal exec -- ghc demo-imgui-builtin.hs -framework OpenGL -lglfw -lGL -package extra && \
#cabal exec -- ghc demo-implot-builtin.hs -framework OpenGL -lglfw -lGL -package extra && \
#./demo-imgui-builtin && \
#./demo-implot-builtin && \
