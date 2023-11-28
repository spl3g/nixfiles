{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "g";
  version = "0.24.2";
  src = fetchFromGitHub {
    owner = "Equationzhao";
    repo = "${pname}";
    rev = "v${version}";
    hash = "sha256-JXNnbw1ckpTe/OHV8E6R1JCmQGfjythzrw67B5ZkQAM=";
  };

  vendorHash = "sha256-vAUPenFdzXXq48fc0EQYQDpiOvpHRz69D8gPVa+6gUI=";

  ldflags = [ "-s" "-v" ];

  postInstall = ''
    mkdir -p $out/usr/share/man/man1
    gzip -d man/g.1.gz
    cp man/g.1 $out/usr/share/man/man1/
  '';
}
