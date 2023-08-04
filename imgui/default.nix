{ stdenv, lib, fetchFromGitHub, frameworks, glfw }:

stdenv.mkDerivation rec {
  pname = "imgui";
  version = "1.89.5";

  src = fetchFromGitHub {
    owner = "ocornut";
    repo = "imgui";
    rev = "v${version}";
    sha256 = "sha256-Ha70CTSBpyF9S+/qG9lAhUlUT4vY0crOoi3vFsy65H8=";
  };

  dontBuild = false; # true;

  buildInputs = [
    glfw
    frameworks.Cocoa
    frameworks.Metal
    frameworks.MetalKit
  ];

  buildPhase = ''
    $CXX -std=c++11 -I. -I./backends -c imgui.cpp
    $CXX -std=c++11 -I. -I./backends -c imgui_demo.cpp
    $CXX -std=c++11 -I. -I./backends -c imgui_draw.cpp
    $CXX -std=c++11 -I. -I./backends -c imgui_tables.cpp
    $CXX -std=c++11 -I. -I./backends -c imgui_widgets.cpp
    $CXX -std=c++11 -I. -I./backends -I${glfw}/include -I/usr/local/include -c backends/imgui_impl_glfw.cpp
    $CXX -std=c++11 -I. -I./backends -ObjC++ -fobjc-weak -fobjc-arc -c backends/imgui_impl_metal.mm

    $AR -rc libimgui.a imgui.o imgui_demo.o imgui_draw.o imgui_tables.o imgui_widgets.o imgui_impl_glfw.o imgui_impl_metal.o
'';

  #installPhase = ''
  #  mkdir -p $out/include/imgui
  #
  #  cp *.h $out/include/imgui
  #  cp *.cpp $out/include/imgui
  #  cp -a backends $out/include/imgui/
  #  cp -a misc $out/include/imgui/
  #'';

  meta = with lib; {
    description = "Bloat-free Graphical User interface for C++ with minimal dependencies";
    homepage = "https://github.com/ocornut/imgui";
    license = licenses.mit;
    maintainers = with maintainers; [ wolfangaukang ];
    platforms = platforms.all;
  };
}
