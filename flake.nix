{
  description = "hgdal";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    fficxx = {
      url = "github:wavewave/fficxx/master";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    fficxx,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        config = {
          packageOverrides = self: {
            imgui = self.callPackage ./imgui/default.nix {
              frameworks = self.darwin.apple_sdk.frameworks;
            };
          };
        };
      };

      haskellOverlay = final: hself: hsuper: {};
      #  (import ./default.nix { pkgs = final; } hself hsuper);

      #fficxx-version = "0.7.0.1";

      hpkgsFor = compiler:
        pkgs.haskell.packages.${compiler}.extend (hself: hsuper: (fficxx.haskellOverlay.${system} pkgs hself hsuper
          //
          # temporarily commented out until the hackage is updated.
          {
            #"fficxx" = hself.callHackage "fficxx" fficxx-version { };
            #"fficxx-runtime" =
            #  hself.callHackage "fficxx-runtime" fficxx-version { };
            #"stdcxx" = hself.callHackage "stdcxx" fficxx-version { };
            #"template" = pkgs.haskell.lib.doJailbreak hsuper.template;
            "ormolu" =
              pkgs.haskell.lib.overrideCabal hsuper.ormolu
              (drv: {enableSeparateBinOutput = false;});
          }
          // haskellOverlay pkgs hself hsuper));

      # TODO: use haskell.packages.(ghc).shellFor
      mkShellFor = compiler: let
        hsenv = (hpkgsFor compiler).ghcWithPackages (p: [
          p.fficxx
          p.fficxx-runtime
          p.stdcxx
          p.dotgen
          p.optparse-applicative
        ]);
        pyenv =
          pkgs.python3.withPackages
          (p: [p.sphinx p.sphinx_rtd_theme p.myst-parser]);
      in
        pkgs.mkShell {
          buildInputs =
            [
              hsenv
              pkgs.cabal-install
              pkgs.imgui
              pkgs.glfw
              pkgs.alejandra
              pkgs.pkgconfig
              pkgs.graphviz
              # this is due to https://github.com/NixOS/nixpkgs/issues/140774
              (hpkgsFor "ghc927").ormolu
            ]
            ++ pkgs.lib.optional pkgs.stdenv.isLinux pkgs.mesa
            ++ pkgs.lib.optionals pkgs.stdenv.isDarwin
            [
              pkgs.darwin.apple_sdk.frameworks.Cocoa
              pkgs.darwin.apple_sdk.frameworks.Metal
              pkgs.darwin.apple_sdk.frameworks.MetalKit
            ];
          shellHook = ''
            export PS1="\n[hs-imgui:\w]$ \0"
            export DYLD_LIBRARY_PATH=${pkgs.imgui}/lib:$DYLD_LIBRARY_PATH
            export IMGUI=${pkgs.imgui}
          '';
        };

      supportedCompilers = ["ghc927" "ghc945" "ghc962"];
    in {
      packages =
        pkgs.lib.genAttrs supportedCompilers (compiler: hpkgsFor compiler)
        // {imgui = pkgs.imgui;};

      inherit haskellOverlay;

      devShells =
        pkgs.lib.genAttrs supportedCompilers (compiler: mkShellFor compiler);
    });
}
