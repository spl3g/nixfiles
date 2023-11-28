{ config, pkgs, lib, inputs, ... }:

let
  username = "jerpo";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  cliPkgs = with pkgs; [
    fd
    glib
    tldr
    wget
    htop
    jq
    bat
    python311
    fzf
    wineWowPackages.full
    wineWowPackages.fonts
    winetricks
    nix-prefetch-scripts
    libtool
    bluetuith
    steam-run
    unzip
    ghostscript
    g-ls
    wayvnc
    draw
    xdragon
  ];
  gamingPkgs = with pkgs; [
    vulkan-tools
    vkd3d
    dxvk
    lutris
    mangohud
    gamemode
    opentabletdriver
    bottles
    steam
  ];
  guiPkgs = with pkgs; [
    ansel
    libreoffice-fresh
    clash-verge
    pokemon-colorscripts
    telegram-desktop
    transmission-gtk
    osu-lazer-bin
    easyeffects
    obs-studio
  ];
  fonts = with pkgs; [
    rubik
    source-code-pro
    material-design-icons
    font-awesome
  ];
  nonListImports = [
    inputs.hyprland.homeManagerModules.default
    inputs.ags.homeManagerModules.default
    ./themes
  ];
  imports = builtins.concatMap import [
    ./programs
    ./services
    ./wm
  ] ++ nonListImports;

in
{
  inherit imports;
  
  home = {
    inherit username homeDirectory;
    stateVersion = "23.05";
    packages = guiPkgs ++ gamingPkgs ++ cliPkgs ++ fonts;
  };

  xdg = {
    inherit configHome;
    enable = true;
  };

  programs.home-manager.enable = true;
}
