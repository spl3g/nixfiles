{ pkgs, ... }:

{
  programs.ags = {
    enable = true;
    configDir = ./bar;
  };
}
