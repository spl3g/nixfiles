{ config, pkgs, lib, inputs, ... }:

let
  username = "jerpo";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  sysPkgs = with pkgs; [
    fd
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
  ];
  yeahGamingPkgs = with pkgs; [
    vulkan-tools
    vkd3d
    dxvk
    lutris
    mangohud
    gamemode
    opentabletdriver
    bottles
  ];
  defaultPkgs = with pkgs; [
    firefox
    source-code-pro
    pokemon-colorscripts
    kitty
    telegram-desktop
    xdragon
    transmission-gtk
    rubik
    osu-lazer
    figma-linux
    easyeffects
    # darktable
    obs-studio
  ];
  nonListImports = [
    inputs.hyprland.homeManagerModules.default
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
    packages = defaultPkgs ++ yeahGamingPkgs ++ sysPkgs;
  };

  xdg = {
    inherit configHome;
    enable = true;
  };

  programs.home-manager.enable = true;

}
