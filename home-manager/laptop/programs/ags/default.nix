{ pkgs, ... }:

{
  programs.ags = {
    enable = false;
    configDir = ./bar;
  };
}
