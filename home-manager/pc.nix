{ pkgs, ... }:

{
  imports = [
    ./general.nix
    ./homeModules/bspwm.nix
  ];

  bspwm.enable = true;
  emacs.package = pkgs.emacs30;
}
