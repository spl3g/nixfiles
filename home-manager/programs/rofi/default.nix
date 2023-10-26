{ pkgs, ... }:

let
  theme = ./squared-material-pine.rasi;
  cycle = true;
  plugins = with pkgs; [
    rofi-emoji
    rofi-calc
  ];
in
{
  home.packages = with pkgs; [ rbw rofi-rbw pinentry ];
  programs.rofi = {
    enable = true;
    inherit theme cycle plugins;
  };
}
