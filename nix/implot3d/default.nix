{
  stdenv,
  lib,
  fetchFromGitHub,
  pkg-config,
  #frameworks,
  glfw,
  imgui,
  implot,
  libGL ? null,
}:
stdenv.mkDerivation rec {
  pname = "implot3d";
  version = "0.3";

  src = fetchFromGitHub {
    owner = "wavewave";
    repo = "implot3d";
    #rev = "v${version}";
    rev = "7b0606a8d949d4997f28c2b376c35fbbecedda46";
    sha256 = "sha256-44BZhLGBheGPW1/m4Rrk9H7OJ1oKbP9IXivVgxuqUVE=";
  };

  nativeBuildInputs = [pkg-config];

  buildInputs =
    [
      glfw
      imgui
      implot
    ]
    ++ (
      if stdenv.isDarwin
      then [
        #frameworks.Cocoa
        #frameworks.Metal
        #frameworks.MetalKit
      ]
      else [libGL]
    );

  buildPhase =
    if stdenv.isDarwin
    then ''
      $CXX -std=c++11 -I. -I./backends -c implot3d.cpp `pkg-config --cflags libimgui libimplot`
      $CXX -std=c++11 -I. -I./backends -c implot3d_demo.cpp `pkg-config --cflags libimgui libimplot`
      $CXX -std=c++11 -I. -I./backends -c implot3d_items.cpp `pkg-config --cflags libimgui libimplot`
      $CXX -std=c++11 -I. -I./backends -c implot3d_meshes.cpp `pkg-config --cflags libimgui libimplot`
      $CXX -dynamiclib -undefined suppress -flat_namespace -install_name $out/lib/libimplot3d.dylib -o libimplot3d.dylib implot3d.o implot3d_demo.o implot3d_items.o implot3d_meshes.o
    ''
    else ''
      $CXX -std=c++11 -I. -I./backends -c implot3d.cpp `pkg-config --cflags libimgui libimplot`
      $CXX -std=c++11 -I. -I./backends -c implot3d_demo.cpp `pkg-config --cflags libimgui libimplot`
      $CXX -std=c++11 -I. -I./backends -c implot3d_items.cpp `pkg-config --cflags libimgui libimplot`
      $CXX -std=c++11 -I. -I./backends -c implot3d_meshes.cpp `pkg-config --cflags libimgui libimplot`
      $CXX -shared -o libimplot3d.so implot3d.o implot3d_demo.o implot3d_items.o implot3d_meshes.o
    '';

  installPhase = ''
    mkdir -p $out/include/implot3d
    mkdir -p $out/lib
    cp *.h $out/include/implot3d
    cp libimplot3d.* $out/lib

    mkdir -p $out/lib/pkgconfig
    cat >> $out/lib/pkgconfig/libimplot3d.pc << EOF
    Name: libimplot3d
    Description: Dear ImPlot3d
    Version: ${version}
    Libs: -L$out/lib -limplot3d
    Cflags: -I$out/include/implot3d
    EOF
  '';

  meta = with lib; {
    description = "Implot3d";
    homepage = "https://github.com/brenocq/implot3d";
    license = licenses.mit;
    maintainers = with maintainers; [ianwookim];
    platforms = platforms.all;
  };
}
