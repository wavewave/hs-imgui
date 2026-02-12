### imgui

genrule(
    name = "imgui_gen",
    srcs = [
        "imgui-gen/Gen.hs",
    ],
    out = "imgui_gen",
    cmd = "$(exe_target @toolchains//:ghc) imgui-gen/Gen.hs -o ${OUT}",
)

genrule(
    name = "generate_imgui",
    srcs = glob(
        ["imgui-gen/template/*"],
    ),
    out = "imgui",
    cmd = "$(location :imgui_gen) gen --template=imgui-gen/template --output=$OUT",
)

genrule(
    name = "build_imgui",
    srcs = [
        ":generate_imgui",
    ],
    out = "imgui",
    cmd = '''
       pushd imgui
       $(exe_target @toolchains//:cabal) build --builddir=dist-newstyle --ghc-options="-framework OpenGL"
       popd
       cp -a imgui/dist-newstyle $OUT
    ''',
)

### implot

genrule(
    name = "implot_gen",
    srcs = [
        "implot-gen/Gen.hs",
    ],
    out = "implot_gen",
    cmd = "$(exe_target @toolchains//:ghc) implot-gen/Gen.hs -o ${OUT}",
)

genrule(
    name = "generate_implot",
    srcs = glob(
        ["implot-gen/template/*"],
    ),
    out = "implot",
    cmd = "$(location :implot_gen) gen --template=implot-gen/template --output=$OUT",
)

genrule(
    name = "build_implot",
    srcs = [
        ":generate_implot",
    ],
    out = "implot",
    cmd = '''
       pushd implot
       $(exe_target @toolchains//:cabal) build --builddir=dist-newstyle \
         --package-db=../$(location :build_imgui)/packagedb/ghc-9.6.7/
       popd
       cp -a implot/dist-newstyle $OUT
    ''',
)

### implot3d

genrule(
    name = "implot3d_gen",
    srcs = [
        "implot3d-gen/Gen.hs",
    ],
    out = "implot3d_gen",
    cmd = "$(exe_target @toolchains//:ghc) implot3d-gen/Gen.hs -o ${OUT}",
)

genrule(
    name = "generate_implot3d",
    srcs = glob(
        ["implot3d-gen/template/*"],
    ),
    out = "implot3d",
    cmd = "$(location :implot3d_gen) gen --template=implot3d-gen/template --output=$OUT",
)

genrule(
    name = "build_implot3d",
    srcs = [
        ":generate_implot3d",
    ],
    out = "implot3d",
    cmd = '''
       pushd implot3d
       $(exe_target @toolchains//:cabal) build --builddir=dist-newstyle \
         --package-db=../$(location :build_imgui)/packagedb/ghc-9.6.7/ \
         --package-db=../$(location :build_implot)/packagedb/ghc-9.6.7/
       popd
       cp -a implot3d/dist-newstyle $OUT
    ''',
)

### DEMO

# imgui-splitter-demo

genrule(
    name = "build_imgui_splitter_demo",
    srcs = glob(
        ["examples/imgui-splitter-demo/**/*"],
    ),
    out = "imgui_splitter_demo",
    cmd = '''
       pushd examples/imgui-splitter-demo
       $(exe_target @toolchains//:cabal) build --builddir=dist-newstyle \
         --ghc-options="-framework OpenGL" \
         --package-db=../../$(location :build_imgui)/packagedb/ghc-9.6.7/ \
         --package-db=../../$(location :build_implot)/packagedb/ghc-9.6.7/
       popd
       cp -a examples/imgui-splitter-demo/dist-newstyle $OUT
    ''',
)

genrule(
    name = "imgui_splitter_demo_sh",
    out = "imgui_splitter_demo.sh",
    cmd = '''
cat > $OUT <<EOF
#!$BASH
set -e
cd \\`dirname "\\$0"\\`
$(location :build_imgui_splitter_demo)/build/aarch64-osx/ghc-9.6.7/imgui-splitter-demo-0.1.0.0/x/imgui-splitter-demo/build/imgui-splitter-demo/imgui-splitter-demo
EOF
chmod +x $OUT
    ''',
)

sh_binary(
    name = "run_imgui_splitter_demo",
    main = ":imgui_splitter_demo_sh",
)

# draw-demo

genrule(
    name = "build_draw_demo",
    srcs = glob(
        ["examples/draw-demo/**/*"],
    ),
    out = "draw_demo",
    cmd = '''
       pushd examples/draw-demo
       $(exe_target @toolchains//:cabal) build --builddir=dist-newstyle \
         --ghc-options="-framework OpenGL" \
         --package-db=../../$(location :build_imgui)/packagedb/ghc-9.6.7/ \
         --package-db=../../$(location :build_implot)/packagedb/ghc-9.6.7/
       popd
       cp -a examples/draw-demo/dist-newstyle $OUT
    ''',
)

genrule(
    name = "draw_demo_sh",
    out = "draw_demo.sh",
    cmd = '''
cat > $OUT <<EOF
#!$BASH
set -e
cd \\`dirname "\\$0"\\`
$(location :build_draw_demo)/build/aarch64-osx/ghc-9.6.7/draw-demo-0.1.0.0/x/draw-demo/build/draw-demo/draw-demo
EOF
chmod +x $OUT
    ''',
)

sh_binary(
    name = "run_draw_demo",
    main = ":draw_demo_sh",
)

# plot-demo

genrule(
    name = "build_plot_demo",
    srcs = glob(
        ["examples/plot-demo/**/*"],
    ),
    out = "plot_demo",
    cmd = '''
       pushd examples/plot-demo
       $(exe_target @toolchains//:cabal) build --builddir=dist-newstyle \
         --ghc-options="-framework OpenGL" \
         --package-db=../../$(location :build_imgui)/packagedb/ghc-9.6.7/ \
         --package-db=../../$(location :build_implot)/packagedb/ghc-9.6.7/
       popd
       cp -a examples/plot-demo/dist-newstyle $OUT
    ''',
)

genrule(
    name = "plot_demo_sh",
    out = "plot_demo.sh",
    cmd = '''
cat > $OUT <<EOF
#!$BASH
set -e
cd \\`dirname "\\$0"\\`
$(location :build_plot_demo)/build/aarch64-osx/ghc-9.6.7/plot-demo-0.1.0.0/x/plot-demo/build/plot-demo/plot-demo
EOF
chmod +x $OUT
    ''',
)

sh_binary(
    name = "run_plot_demo",
    main = ":plot_demo_sh",
)

# plot3d-demo

genrule(
    name = "build_plot3d_demo",
    srcs = glob(
        ["examples/plot3d-demo/**/*"],
    ),
    out = "plot3d_demo",
    cmd = '''
       pushd examples/plot3d-demo
       $(exe_target @toolchains//:cabal) build --builddir=dist-newstyle \
         --ghc-options="-framework OpenGL" \
         --package-db=../../$(location :build_imgui)/packagedb/ghc-9.6.7/ \
         --package-db=../../$(location :build_implot)/packagedb/ghc-9.6.7/ \
         --package-db=../../$(location :build_implot3d)/packagedb/ghc-9.6.7/
       popd
       cp -a examples/plot3d-demo/dist-newstyle $OUT
    ''',
)

genrule(
    name = "plot3d_demo_sh",
    out = "plot3d_demo.sh",
    cmd = '''
cat > $OUT <<EOF
#!$BASH
set -e
cd \\`dirname "\\$0"\\`
$(location :build_plot3d_demo)/build/aarch64-osx/ghc-9.6.7/plot3d-demo-0.1.0.0/x/plot3d-demo/build/plot3d-demo/plot3d-demo
EOF
chmod +x $OUT
    ''',
)

sh_binary(
    name = "run_plot3d_demo",
    main = ":plot3d_demo_sh",
)
