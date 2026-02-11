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
