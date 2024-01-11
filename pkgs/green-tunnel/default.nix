{ buildNpmPackage, fetchFromGitHub, lib }:

buildNpmPackage rec {
  name = "GreenTunnel";
  version = "1.8.3";
  
  src = fetchFromGitHub {
    repo = name;
    owner = "SadeghHayeri";
    rev = "v${version}";
    sha256 = "06c90x2ykm350v2fd1zny0bh5s7hbchx457n916lgcbzzdq7yr6r";
  };

  npmDepsHash = "sha256-Z+oJ0ZySQA2rFz/l6HSY0QzZodKxQ9jm8h+D6m3JzV0=";
  dontNpmBuild = true;
}
