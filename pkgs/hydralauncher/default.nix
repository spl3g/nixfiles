{ lib
, appimageTools
, fetchurl
}:

appimageTools.wrapType2 rec {
  pname = "hydralauncher";
  version = "2.0.2";

  src = fetchurl {
    url = "https://github.com/hydralauncher/hydra/releases/download/v${version}/hydralauncher-${version}.AppImage";
    sha256 = "0kmfm8ppf4ibhfsg5ihvzzx7apc7bd7dvz4zays14h15dgzhi5jc";
  };
}
