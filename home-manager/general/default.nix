{ pkgs, ... }:
let 
  username = "jerpo";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  
  cliPkgs = with pkgs; [
    gcc
    cmake
    gnumake
    fd
    pkg-config
    glib
    tldr
    wget
    btop
    # jq
    bat
    python311
    fzf
    nix-prefetch-scripts
    libtool
    steam-run
    unzip
    ghostscript
    xdragon
  ];
  gamingPkgs = with pkgs; [
    vkd3d
    # mangohud
    gamemode
    # opentabletdriver
    bottles
    steam
  ];
   guiPkgs = with pkgs; [
    libreoffice-fresh
    clash-verge
    pokemon-colorscripts
    telegram-desktop
    transmission-gtk
    osu-lazer-bin
    easyeffects
  ];
  fonts = with pkgs; [
    rubik
    source-code-pro
    material-design-icons
    font-awesome
  ];
  imports = builtins.concatMap import [
    ./programs
  ] ++ [ ./themes ];
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
