{ lib, stdenv, fetchFromGitLab, python3 }:

stdenv.mkDerivation rec {
  pname = "pokemon-colorscripts";
  version = "r108.3dc0b2e";
  src = fetchFromGitLab {
    owner = "phoneybadger";
    repo = "${pname}";
    rev = "0483c85b93362637bdd0632056ff986c07f30868";
    sha256 = "1c5pi7y87ng6qa3k287fq66spdl3ynckvxf86srd9fy2h4ljlgdf";
  };
  buildInputs = [ python3 ];
  preInstall = ''
    rm -rf "$out/usr/local/opt/${pname}"
    mkdir -p "$out/usr/local/opt/${pname}/colorscripts/regular"
    mkdir -p "$out/usr/local/opt/${pname}/colorscripts/shiny"
    mkdir -p "$out/bin"
    install -Dm644 colorscripts/small/regular/* -t "$out/usr/local/opt/${pname}/colorscripts/small/regular"
    install -Dm644 colorscripts/small/shiny/* -t "$out/usr/local/opt/${pname}/colorscripts/small/shiny"
    install -Dm644 colorscripts/large/regular/* -t "$out/usr/local/opt/${pname}/colorscripts/large/regular"
    install -Dm644 colorscripts/large/shiny/* -t "$out/usr/local/opt/${pname}/colorscripts/large/shiny"
    install -Dm644 pokemon.json "$out/usr/local/opt/${pname}/pokemon.json"
    install -Dm755 pokemon-colorscripts.py "$out/usr/local/opt/${pname}/pokemon-colorscripts.py"
    install -Dm644 LICENSE.txt "$out/usr/share/licenses/${pname}/LICENSE"
    install -Dm644 README.md "$out/usr/share/doc/${pname}/README.md"
    install -Dm644 pokemon-colorscripts.1 "$out/usr/local/man/man1/pokemon-colorscripts.1"
    ln -sf $out/usr/local/opt/${pname}/pokemon-colorscripts.py $out/bin/pokemon-colorscripts
'';
}
