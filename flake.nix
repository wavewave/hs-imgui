{
  description = "hs-imgui";
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
            imgui = self.callPackage ./nix/imgui/default.nix {
              frameworks = self.darwin.apple_sdk.frameworks;
            };
            implot = self.callPackage ./nix/implot/default.nix {
              frameworks = self.darwin.apple_sdk.frameworks;
            };
          };
        };
      };

      haskellOverlay = final: hself: hsuper:
        (import ./imgui-gen/default.nix {pkgs = final;} hself hsuper)
        // (import ./implot-gen/default.nix {pkgs = final;} hself hsuper);

      hpkgsFor = compiler:
        pkgs.haskell.packages.${compiler}.extend (hself: hsuper: (fficxx.haskellOverlay.${system} pkgs hself hsuper
          // haskellOverlay pkgs hself hsuper));

      # TODO: use haskell.packages.(ghc).shellFor
      mkShellFor = compiler: let
        hsenv = (hpkgsFor compiler).ghcWithPackages (p: [
          p.fficxx
          p.fficxx-runtime
          p.stdcxx
          p.dotgen
          p.optparse-applicative
	  p.imgui
	  p.implot
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
              pkgs.implot
              pkgs.glfw
              pkgs.alejandra
              pkgs.pkgconfig
              pkgs.graphviz
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux
	    [ pkgs.mesa
	      pkgs.xorg.libX11
	      pkgs.xorg.libXau
	      pkgs.xorg.libXdmcp
	    ]
            ++ pkgs.lib.optionals pkgs.stdenv.isDarwin
            [
              pkgs.darwin.apple_sdk.frameworks.Cocoa
              pkgs.darwin.apple_sdk.frameworks.Metal
              pkgs.darwin.apple_sdk.frameworks.MetalKit
            ];
          shellHook = ''
            export PS1="\n[hs-imgui:\w]$ \0"
          '';
        };

      supportedCompilers = ["ghc962"];
      defaultCompiler = "ghc962";
    in rec {
      packages =
        pkgs.lib.genAttrs supportedCompilers (compiler: hpkgsFor compiler)
        // {
          inherit (pkgs) imgui implot;
        };

      inherit haskellOverlay;

      devShells =
        pkgs.lib.genAttrs supportedCompilers (compiler: mkShellFor compiler)
        // {
          default = devShells.${defaultCompiler};
        };
    });
}
