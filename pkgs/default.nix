{ pkgs ? import <nixpkgs> { } }: {
  sddm-sugar-dark-theme = pkgs.libsForQt5.callPackage ./sddm-sugar-dark-theme { };
  tgs2png = pkgs.callPackage ./tgs2png { };
  pokemon-colorscripts = pkgs.callPackage ./pokemon-colorscripts { };
}
