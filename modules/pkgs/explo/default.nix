let
  explo = {
    lib,
    buildGoModule,
    fetchFromGitHub,
    jellyfin-ffmpeg,
    yt-dlp,
  }:
    buildGoModule rec {
      pname = "explo";
      version = "0.11.5";

      src = fetchFromGitHub {
        owner = "LumePart";
        repo = "Explo";
        rev = "v${version}";
        sha256 = "sha256-A3ikFH0/C/dat1pf7t1Gp6bfitmbPHK+RKVzqsLzjc0=";
      };

      buildInputs = [jellyfin-ffmpeg yt-dlp];

      vendorHash = "sha256-jTvxv0cyE/+BNkrajIj8E3xlftq+PCtGbmz+P3IuMFw=";
    };
in {
  perSystem = {pkgs, ...}: {
    packages.explo = pkgs.callPackage explo {};
  };
}
