{stdenv}: hself: let
  hsenv = hself.ghcWithPackages (p: with p; [fficxx fficxx-runtime optparse-applicative]);
in
  stdenv.mkDerivation {
    name = "implot-src";
    buildInputs = [hsenv];
    src = ./.;
    buildPhase = ''
      ghc Gen.hs
      ./Gen gen -t ./template
    '';
    installPhase = ''
      mkdir -p $out
      cp -a implot/* $out
    '';
  }
