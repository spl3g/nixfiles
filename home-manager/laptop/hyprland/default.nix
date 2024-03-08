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
    grimblast
    cliphist
    polkit_gnome
    xwaylandvideobridge
  ];

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-hyprland
    ];
    configPackages = with pkgs; [
      xdg-desktop-portal-hyprland
    ];
  };

  programs.fish.loginShellInit = "Hyprland";
  
  wayland.windowManager.hyprland = {
    enable = true;
    package = pkgs.hyprland;
    inherit extraConfig;
  };

  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings = {
      indicator-radius = 100;
      ring-color = "000000ff";
      layout-bg-color = "000000ff";
      key-hl-color = "e0c69fff";
      inside-color = "000000ff";
      layout-border-color = "e0c69fff";
      line-color = "ffffffff";
      separator-color = "000000ff";
      ring-ver-color = "000000ff";
      line-ver-color = "000000ff";
      inside-ver-color = "ffffff00";
      text-ver-color = "e0c69fff";
      inside-wrong-color = "ffffff00";
      text-wrong-color = "e0c69fff";
      ring-wrong-color = "762f20ff";
      fade-in = 1;
    };
  };
}
