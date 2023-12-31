{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
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
  };
}
