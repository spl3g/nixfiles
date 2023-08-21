{ pkgs ? import <nixpkgs> { } }: {
  sddm-sugar-dark-theme = pkgs.libsForQt5.callPackage ./sddm-sugar-dark-theme { };
  minicava = pkgs.callPackage ./minicava { };  
}
