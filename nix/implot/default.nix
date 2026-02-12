{
  stdenv,
  lib,
  fetchFromGitHub,
  pkg-config,
  #frameworks,
  imgui,
  glfw,
  libGL ? null,
}:
stdenv.mkDerivation rec {
  pname = "implot";
  version = "0.15";

  src = fetchFromGitHub {
    owner = "epezent";
    repo = "implot";
    rev = "v${version}";
    sha256 = "sha256-sCMkZREX/O852jDIoa1zLcP8DN1VcrL+EM3+QDAnliA=";
  };

  nativeBuildInputs = [pkg-config];

  buildInputs =
    [
      imgui
      glfw
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
      $CXX -std=c++11 -I. -I./backends -c implot.cpp `pkg-config --cflags libimgui`
      $CXX -std=c++11 -I. -I./backends -c implot_demo.cpp `pkg-config --cflags libimgui`
      $CXX -std=c++11 -I. -I./backends -c implot_items.cpp `pkg-config --cflags libimgui`
      $CXX -dynamiclib -undefined suppress -flat_namespace -install_name $out/lib/libimplot.dylib -o libimplot.dylib implot.o implot_demo.o implot_items.o
    ''
    else ''
      $CXX -std=c++11 -I. -I./backends -c implot.cpp `pkg-config --cflags libimgui`
      $CXX -std=c++11 -I. -I./backends -c implot_demo.cpp `pkg-config --cflags libimgui`
      $CXX -std=c++11 -I. -I./backends -c implot_items.cpp `pkg-config --cflags libimgui`
      $CXX -shared -o libimplot.so implot.o implot_demo.o implot_items.o
    '';

  installPhase = ''
    mkdir -p $out/include/implot
    mkdir -p $out/lib
    cp *.h $out/include/implot
    cp libimplot.* $out/lib

    mkdir -p $out/lib/pkgconfig
    cat >> $out/lib/pkgconfig/libimplot.pc << EOF
    Name: libimplot
    Description: Dear ImPlot
    Version: ${version}
    Libs: -L$out/lib -limplot
    Cflags: -I$out/include/implot
    EOF
  '';

  meta = with lib; {
    description = "mPlot is an immediate mode, GPU accelerated plotting library for Dear ImGui.";
    homepage = "https://github.com/epezent/implot";
    license = licenses.mit;
    maintainers = with maintainers; [ianwookim];
    platforms = platforms.all;
  };
}
