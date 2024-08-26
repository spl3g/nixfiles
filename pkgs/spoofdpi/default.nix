{ lib
, buildGoModule
, fetchFromGitHub
}:

buildGoModule rec {
  pname = "spoofdpi";
  version = "0.10.6";

  src = fetchFromGitHub {
    owner = "xvzc";
    repo = "SpoofDPI";
    rev = "v${version}";
    sha256 = "sha256-DsyUxeVLJHnNZM92z4DEJPIy0HWD9WXBJ+HordPSYMg=";
  };

  vendorHash = "sha256-kmp+8MMV1AHaSvLnvYL17USuv7xa3NnsCyCbqq9TvYE=";  
}
