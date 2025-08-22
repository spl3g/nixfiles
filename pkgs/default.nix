{ pkgs  }: {
  tgs2png = pkgs.callPackage ./tgs2png { };
  pokemon-colorscripts = pkgs.callPackage ./pokemon-colorscripts { };
  spoofdpi = pkgs.callPackage ./spoofdpi { };
  boomer = pkgs.callPackage ./boomer/default.nix { nim = pkgs.nim-1_0; };
}
