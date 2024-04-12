{ lib
, appimageTools
, fetchurl
}:

appimageTools.wrapType2 rec {
  name = "spmp";
  version = "0.3.1";
  src = fetchurl {
    url = "https://github.com/toasterofbread/spmp/releases/download/v${version}/spmp-v${version}-linux-x86_64.appimage";
    sha256 = "00nmz1v4w1l7qw0c1pm9vibklpvq1dg0npgvhlvncm6djiqs4af6";
  };
  extraPkgs = pkgs: with pkgs; [
    mpv
    libxcrypt-legacy
    zulu
    curl
    libappindicator-gtk3
    libdbusmenu
  ];
}
