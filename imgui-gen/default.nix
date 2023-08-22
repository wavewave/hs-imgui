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
    libX11 ? null,
    libXau ? null,
    libXdmcp ? null,
  }:
    mkDerivation {
      pname = "imgui";
      version = "1.0.0.0";
      src = imgui-src;
      libraryToolDepends = [pkg-config];
      libraryHaskellDepends = [base fficxx-runtime stdcxx];
      librarySystemDepends = [imguiLib glfw3 libX11 libXau libXdmcp];
      license = pkgs.lib.licenses.bsd3;
    }) ({imguiLib = pkgs.imgui;}
    // (
      if pkgs.stdenv.isLinux
      then {
        inherit (pkgs.xorg) libX11 libXau libXdmcp;
      }
      else {}
    ));
}
