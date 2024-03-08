{ stdenv
, lib
, fetchFromGitHub
, cmake
, pkg-config
, libsecret
, libgcrypt
, openssl
, openvpn
, tun2socks
, cloak
, shadowsocks-libev
, wireguard-go
, xray
, qtbase
, wrapQtAppsHook
, qtremoteobjects
, qtsvg
, qttools
, qt5compat
, qtkeychain
}:

stdenv.mkDerivation rec {
  pname = "amnezia-client";
  version = "4.4.0.0";
  
  src = fetchFromGitHub {
    repo = "${pname}";
    owner = "amnezia-vpn";

    rev = "${version}";
    sha256 = "0k0sjf1c8iqg9crq4sfzdg4cczaq5pbk5j2873yzsw88rps0awac";
    fetchSubmodules = true;
  };
  
  nativeBuildInputs = [
    cmake
    wrapQtAppsHook
    pkg-config
  ];

  buildInputs = [
    qtbase
    qtremoteobjects
    qtsvg
    qttools
    qt5compat
    qtkeychain
    libsecret
    libgcrypt
    openssl
    openvpn
    tun2socks
    cloak
    shadowsocks-libev
    wireguard-go
    xray
  ];

  installPhase = ''
  runHook preInstall
  make install
  mkdir $out/bin
  mkdir -p $out/usr/share/{pixmaps,applications}
  cp client/AmneziaVPN $out/bin/
  cp service/server/AmneziaVPN-service $out/bin/
  runHook postInstall
  '';
}
