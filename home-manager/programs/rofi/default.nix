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
  programs.rofi = {
    enable = true;
    inherit theme cycle plugins;
  };
}
