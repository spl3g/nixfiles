{ pkgs, ... }:
let
  configFile = builtins.readFile ./hyprland.conf;
  scripts = ''
    $scripts=${./scripts}
  '';
  overrides = ''
    exec-once=swww init && swww img ${./cat.png}
  '';
  extraConfig = scripts + configFile + overrides;
  
in
{
  home.packages = with pkgs; [
    swww
    brightnessctl
    slurp
    grim
    cliphist
    xdg-desktop-portal-hyprland
  ];
  wayland.windowManager.hyprland = {
    enable = true;
    package = pkgs.hyprland;
    inherit extraConfig;
  };
}
