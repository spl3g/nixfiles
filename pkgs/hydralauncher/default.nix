{ lib
, fetchFromGitHub
, mkYarnPackage
, fetchYarnDeps
, hydralauncher-bittorrent-client ? import ./client.nix
, electron
}:

mkYarnPackage rec {
  name = "hydralauncher";
  version = "2.0.2";
  
  src = fetchFromGitHub {
    repo = "hydra";
    owner = "hydralauncher";
    rev = "v${version}";
    sha256 = "1g1a03cay4vw32jl6jm94gfaf099q3ln1kcjy52g6z4r4qmjkgch";
  };
  
  offlineCache = fetchYarnDeps {
    yarnLock = src + "/yarn.lock";
    hash = "sha256-IzpsUkbLmY1pwooZiIvlTmGFSzOoRbPzBRYP55OImyc=";
  };
  yarnLock = src + "/yarn.lock";
  packageJSON = src + "/package.json";

  buildInputs = [ hydralauncher-bittorrent-client ];
  
  nativeBuildInputs = [
    electron
  ];

  buildPhase = ''
    export HOME=$(mktemp -d)
    yarn --offline build:linux
  '';

  postInstall = ''
  makeWrapper ${electron}/bin/electron $out/bin/hydralauncher \
    --add-flags $out/share
  '';
}
