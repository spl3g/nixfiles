{ pkgs, config, ... }:
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
    eww-wayland
    socat
    inotify-tools
    brightnessctl
    alsa-utils
    slurp
    grim
    cliphist
    material-design-icons
    font-awesome
    wtype
    webcord
    xdg-desktop-portal-hyprland
    swaylock
  ];
  wayland.windowManager.hyprland = {
    enable = true;
    package = pkgs.hyprland;
    recommendedEnvironment = true;
    inherit extraConfig;
  };
}
