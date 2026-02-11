native.genrule(
    name = "imgui_gen",
    srcs = [
        "imgui-gen/Gen.hs",
    ],
    out = "imgui_gen",
    cmd = "$(exe_target @toolchains//:ghc) imgui-gen/Gen.hs -o ${OUT}",
)

native.genrule(
    name = "generate_imgui",
    srcs = glob(
        ["imgui-gen/template/*"],
    ),
    out = "imgui",
    cmd = "$(location :imgui_gen) gen --template=imgui-gen/template --output=$OUT",
)
