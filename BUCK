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
         --package-db=../$(location :build_imgui)/packagedb/ghc-9.6.2/
       popd
       cp -a implot/dist-newstyle $OUT
    ''',
)

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
         --package-db=../$(location :build_imgui)/packagedb/ghc-9.6.2/ \
         --package-db=../$(location :build_implot)/packagedb/ghc-9.6.2/
       popd
       cp -a implot3d/dist-newstyle $OUT
    ''',
)

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
         --package-db=../../$(location :build_imgui)/packagedb/ghc-9.6.2/ \
         --package-db=../../$(location :build_implot)/packagedb/ghc-9.6.2/ \
         --package-db=../../$(location :build_implot3d)/packagedb/ghc-9.6.2/
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
$(location :build_plot3d_demo)/build/aarch64-osx/ghc-9.6.2/plot3d-demo-0.1.0.0/x/plot3d-demo/build/plot3d-demo/plot3d-demo
EOF
chmod +x $OUT
    ''',
)

sh_binary(
    name = "run_plot3d_demo",
    main = ":plot3d_demo_sh",
)
