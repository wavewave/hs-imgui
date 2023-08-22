{pkgs}: hself: hsuper: let
  imgui-src = (pkgs.callPackage ./gen.nix {}) hself;
in rec {
  # Haskell binding library
  "imgui" = hself.callPackage ({
    mkDerivation,
    base,
    fficxx-runtime,
    stdenv,
    template-haskell,
    stdcxx,
    pkg-config,
    imguiLib,
    glfw3,
  }:
    mkDerivation {
      pname = "imgui";
      version = "1.0.0.0";
      src = imgui-src;
      libraryToolDepends = [pkg-config];
      libraryHaskellDepends = [base fficxx-runtime stdcxx];
      librarySystemDepends = [imguiLib glfw3];
      license = pkgs.lib.licenses.bsd3;
    }) {imguiLib = pkgs.imgui;};
}
