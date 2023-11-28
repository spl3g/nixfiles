{ lib
, stdenv
, fetchgit
, cmake
, intltool
, wrapGAppsHook
, ocl-icd
, desktop-file-utils
, llvmPackages_13
, python311Packages
, clang
, portmidi
, libwebp
, pkg-config
, gettext
, perl
, libxml2
, pugixml
, libjpeg
, colord-gtk
, colord
, openexr_3
, lensfun
, isocodes
, zlib
, exiv2
, openjpeg
, graphicsmagick
, lua
, osm-gps-map
, libsecret
, gmic
, libavif
, jasper
, libjxl
, libxslt
, saxon
, exiftool
, pcre2
, libheif
, sqlite
, librsvg
, curl
, opencl-clang
, lcms
, json-glib
, ninja
, gdk-pixbuf
, glib
, atkmm
, cairo
, cmocka
, cups
, gtk3
, pango
, libpng
, libsoup
, libtiff
}:

stdenv.mkDerivation rec {
  name = "ansel";
  version = "06d9cda";
  src = fetchgit {
    url = "https://github.com/aurelienpierreeng/${name}.git";
    rev = "06d9cda326f37dafe7e315a5440c8b8b78333ee1";
    hash = "sha256-b/O+ApenIphKJp/20ROy7/drQuo3mqQEvrkYDrSMxyc=";
    fetchSubmodules = true;
  };
  nativeBuildInputs = [
    gtk3
    cups
    cmake
    intltool
    desktop-file-utils
    llvmPackages_13.llvm
    pkg-config
    perl
    llvmPackages_13.clang
    opencl-clang
    ninja
    wrapGAppsHook
  ];
    
  buildInputs = [
    libtiff
    libsoup
    pango
    libpng
    cmocka
    python311Packages.jsonschema
    cairo
    atkmm
    gdk-pixbuf
    json-glib
    lcms
    curl
    librsvg
    sqlite
    exiftool
    libheif
    pcre2
    saxon
    libxslt
    libwebp
    pugixml
    libxml2
    libjpeg
    colord-gtk
    colord
    openexr_3
    lensfun
    isocodes
    zlib
    exiv2
    openjpeg
    graphicsmagick
    lua
    osm-gps-map
    libsecret
    llvmPackages_13.openmp
    gmic
    libavif
    jasper
    libjxl
  ];
  cmakeFlags = [
    "-G Ninja"
    "-DCMAKE_BUILD_TYPE=Release"
    "-DBINARY_PACKAGE_BUILD=ON"
    "-DCMAKE_INSTALL_LIBDIR=lib64"
  ];
  
  preFixup =
    let
      libPathEnvVar = "LD_LIBRARY_PATH";
      libPathPrefix = "$out/lib/ansel:${ocl-icd}/lib";
    in
      ''
        for f in $out/share/ansel/kernels/*.cl; do
          sed -r "s|#include \"(.*)\"|#include \"$out/share/ansel/kernels/\1\"|g" -i "$f"
        done

        gappsWrapperArgs+=(
          --prefix ${libPathEnvVar} ":" "${libPathPrefix}"
        )
      '';
  
  meta = with lib; {
    description = "Photo-editing software for digital artists";
    homepage = "https://ansel.photos/";
    license = licenses.gpl3Plus;
    platforms = platforms.linux;
    maintainers = with maintainers; [ spl3g ];
  };

}


