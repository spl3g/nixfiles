{ lib
, stdenv
, fetchFromGitHub
, rlottie
, libpng
, cmake
, pkg-config
}:

stdenv.mkDerivation {
  name = "tgs2png";
  src = fetchFromGitHub {
    owner = "zevlg";
    repo = "tgs2png";
    rev = "25c15b7c2ca3b1a580a383d9d3cb13bf8531d04a";
    sha256 = "0camvzapkfvr9v0nkk96n26rdmw0g8wbpv41i5l03j6bzdgm4myl";
  };
  nativeBuildInputs = [ cmake pkg-config ];
  buildInputs = [
    rlottie
    libpng
  ];
  buildPhase = ''
    cmake
    make
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp tgs2png $out/bin
  '';
}
    
