{ pkgs, ... }:

let
  theme = ./squared-material-pine.rasi;
  cycle = true;
  plugins = with pkgs; [
    rofi-emoji
    rofi-calc
  ];
  extraConfig = {
    kb-row-up = "Up,Alt+k";
    kb-row-down = "Down,Alt+j";
  };
in
{
  home.packages = with pkgs; [ rbw rofi-rbw pinentry ];
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
    inherit theme cycle plugins extraConfig;
  };
}
