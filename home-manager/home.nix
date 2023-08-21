{ config, pkgs, lib, inputs, ... }:

let
  username = "jerpo";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  sysPkgs = with pkgs; [
    ripgrep
    fd
    tldr
    unzip
    wget
    htop
    jq
    bat
    rustup
    python3Full
    silver-searcher
    fzf
    wineWowPackages.full
    wineWowPackages.fonts
    winetricks
    pamixer
    nix-prefetch-scripts
    libtool
  ];
  yeahGamingPkgs = with pkgs; [
    vkd3d
    nvtop-nvidia
    dxvk
    lutris
    steam
    mangohud
    gamemode
  ];
  defaultPkgs = with pkgs; [
    firefox
    minicava
    droidcam
    source-code-pro
    pokemon-colorscripts-mac
    kitty
    libreoffice
    telegram-desktop
    feh
    betterlockscreen
    xdragon
    obsidian
    xclip
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
    packages = defaultPkgs ++ pythonPkgs ++ yeahGamingPkgs ++ sysPkgs;
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
