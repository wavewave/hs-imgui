{pkgs}: hself: hsuper: let
  implot-src = (pkgs.callPackage ./gen.nix {}) hself;
in rec {
  # Haskell binding library
  "implot" = hself.callPackage ({
    mkDerivation,
    base,
    fficxx-runtime,
    stdenv,
    template-haskell,
    stdcxx,
    pkg-config,
    imgui,
    implotLib,
    imguiLib,
    glfw3,
  }:
    mkDerivation {
      pname = "implot";
      version = "1.0.0.0";
      src = implot-src;
      libraryToolDepends = [pkg-config];
      libraryHaskellDepends = [base fficxx-runtime stdcxx imgui];
      librarySystemDepends = [implotLib imguiLib glfw3];
      license = pkgs.lib.licenses.bsd3;
    }) {
    implotLib = pkgs.implot;
    imguiLib = pkgs.imgui;
  };
}
