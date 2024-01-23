{ pkgs, ... }:
let 
  username = "jerpo";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  
  cliPkgs = with pkgs; [
    fd
    tldr
    wget
    btop
    # jq
    bat
    fzf
    nix-prefetch-scripts
    steam-run
    unzip
    ghostscript
    xdragon
  ];
  gamingPkgs = with pkgs; [
    vkd3d
    mangohud
    gamemode
    bottles
    steam
  ];
   guiPkgs = with pkgs; [
    libreoffice-fresh
    pokemon-colorscripts
    telegram-desktop
    transmission-gtk
    osu-lazer-bin
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
