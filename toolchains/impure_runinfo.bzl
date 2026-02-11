def _impure_runinfo_impl(ctx: AnalysisContext) -> list[Provider]:
    executable = cmd_args(ctx.attrs.executable)
    run_info = RunInfo(args = executable)
    return [DefaultInfo(), run_info]

impure_runinfo = rule(
    impl = _impure_runinfo_impl,
    attrs = {
        "executable": attrs.string(default = "dummy")
    },
)
