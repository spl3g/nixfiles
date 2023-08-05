{ lib, stdenv, fetchFromGitHub, qtsvg, qtquickcontrols2, qtgraphicaleffects }:

stdenv.mkDerivation {
  pname = "sddm-sugar-dark-theme";
  version = "1.2";

  src = fetchFromGitHub {
    owner = "MarianArlt";
    repo = "sddm-sugar-dark";
    rev = "ceb2c455663429be03ba62d9f898c571650ef7fe";
    sha256 = "0153z1kylbhc9d12nxy9vpn0spxgrhgy36wy37pk6ysq7akaqlvy";
  };
  propagatedBuildInputs = [
    qtsvg
    qtquickcontrols2
    qtgraphicaleffects
  ];
  dontWrapQtApps = true;
  postInstall = ''
    mkdir -p $out/share/sddm/themes/sugar-dark
    mv * $out/share/sddm/themes/sugar-dark/
  '';
  postFixup = ''
    echo ${qtgraphicaleffects} >> $out/nix-support/propageted-user-env-packages
    echo ${qtsvg} >> $out/nix-support/propageted-user-env-packages
    echo ${qtquickcontrols2} >> $out/nix-support/propageted-user-env-packages
  '';
}
