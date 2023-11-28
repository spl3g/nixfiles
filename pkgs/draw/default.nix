{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "draw";
  version = "0.1.1";
  
  src = fetchFromGitHub {
    owner = "maaslalani";
    repo = "${pname}";
    rev = "v${version}";
    hash = "sha256-ItrIN2Jyk+aoG2xxHT546GBuUV7yQ+saypEZKSEdRmw=";
  };

  vendorHash = "sha256-wUGxg+cdNY5ekViN5nCuKAVEeTmUbiyrXsuwfhI/2iw=";
}
