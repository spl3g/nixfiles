{ config, pkgs, lib, inputs, ... }:

let
  username = "jerpo";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  defaultPkgs = with pkgs; [
    bat
    emacs
    nerdfonts
    pokemon-colorscripts-mac
    kitty
    telegram-desktop
    feh
    htop
    ranger
    betterlockscreen
    polybar
    xdragon
    obsidian
    xclip
    lutris
    steam
    mangohud
    gamemode
    vkdt
    transmission-gtk
    rubik
    obsidian
    osu-lazer
    spotify
    figma-linux
    easyeffects
    darktable
  ];
  pythonPkgs = with pkgs.python311Packages; [
    python-lsp-server
    pyls-flake8
    python-lsp-black
  ];
in
{
  home = {
    inherit username homeDirectory;
    stateVersion = "23.05";
    packages = defaultPkgs ++ pythonPkgs;
    sessionVariables = {
      DISPLAY = ":0";
    };
  };

  xdg = {
    inherit configHome;
    enable = true;
  };

  programs.home-manager.enable = true;

  imports = builtins.concatMap import [
    ./programs
    ./services
    ./wm
  ];
}
