{ fetchFromGitHub, buildGoModule, lib }:

buildGoModule rec {
  name = "SpoofDPI";
  version = "0.8";
  
  src = fetchFromGitHub {
    repo = name;
    owner = "xvzc";
    rev = "363f022c91773acd62f306ba9bae5bfae4913c2a";
    sha256 = "0gyp4411brdfa9wybgn2z4ljnylid0r4xx6ydy0v56vqk4wrvw4h";
  };

  vendorHash = "sha256-ib9xRklkLfrDCuLf7zDkJE8lJiNiUMPZ01MDxvqho6o=";
}
