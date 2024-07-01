{ fetchFromGitHub
, python39
, ensureNewerSourcesHook
}:

python39.pkgs.buildPythonApplication rec {
  name = "hydralauncher-bittorrent-client";
  version = "2.0.2";
  
  src = fetchFromGitHub {
    repo = "hydra";
    owner = "hydralauncher";
    rev = "v${version}";
    sha256 = "1g1a03cay4vw32jl6jm94gfaf099q3ln1kcjy52g6z4r4qmjkgch";
  };

  format = "other";

  buildInputs = [
    (ensureNewerSourcesHook { year = "1980"; })
  ];
  
  propagatedBuildInputs = with python39.pkgs; [
    cx-freeze
    libtorrent-rasterbar
  ];

  buildPhase = ''
    python torrent-client/setup.py build
  '';
}
