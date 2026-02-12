# Nix expression to build Buck2 from source.
# Based, in part, on https://github.com/thoughtpolice/buck2-nix/blob/c602d0f44f03310a89f209a322bb122b0d3c557a/buck/nix/buck2/default.nix
#
{
  lib,
  fetchFromGitHub,
  makeBinaryWrapper,
  installShellFiles,
  fenix,
  makeRustPlatform,
  openssl,
  pkg-config,
  protobuf,
  sqlite,
  fetchpatch,
  watchman,
}:
let
  rustPlatform = makeRustPlatform {
    cargo = toolchain;
    rustc = toolchain;
  };
  pname = "buck2";
  git_rev = "2025-06-15";

  src = fetchFromGitHub {
    owner = "facebook";
    repo = pname;
    rev = git_rev;
    hash = "sha256-vVtevRuE64AirfOfrSQN7MyFFWQBhbfLSc9MSVcrR8k=";
  };

  toolchain = fenix.fromToolchainFile {
    dir = src;
    sha256 = "sha256-zA9ZxrIQn1RzSn83jL64z4UtnLTY2d1jRI/IfORevLk=";
  };
in
rustPlatform.buildRustPackage {
  inherit pname src;
  version = "git-${git_rev}";

  cargoLock = {
    lockFile = ./Cargo.lock;
    allowBuiltinFetchGit = true;
  };

  postPatch = ''
    cp ${./Cargo.lock} Cargo.lock
    chmod +w Cargo.lock  # Huh???
  '';

  nativeBuildInputs = [
    installShellFiles
    protobuf
    pkg-config
    makeBinaryWrapper
  ];

  buildInputs = [
    openssl
    sqlite
  ];

  BUCK2_BUILD_PROTOC = "${protobuf}/bin/protoc";
  BUCK2_BUILD_PROTOC_INCLUDE = "${protobuf}/include";

  doCheck = false;
  dontStrip = true; # XXX (aseipp): cargo will delete dwarf info but leave symbols for backtraces

  postInstall = ''
    mv $out/bin/buck2     $out/bin/buck
    ln -sfv $out/bin/buck $out/bin/buck2
    mv $out/bin/starlark  $out/bin/buck2-starlark
    mv $out/bin/read_dump $out/bin/buck2-read_dump

    installShellCompletion --cmd buck2 \
      --bash <( $out/bin/buck2 completion bash ) \
      --fish <( $out/bin/buck2 completion fish ) \
      --zsh <( $out/bin/buck2 completion zsh )
  '';

  meta = with lib; {
    description = "Build system, successor to Buck";
    homepage = "https://buck2.build/";
    changelog = "https://github.com/facebook/buck2/blob/main/CHANGELOG.md";
    license = licenses.asl20;
    maintainers = [ ];
    platforms = platforms.linux ++ platforms.darwin;
    mainProgram = "buck2";
  };
}
