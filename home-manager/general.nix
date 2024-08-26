{ pkgs, inputs, outputs, config, lib, ... }:
let 
  username = "jerpo";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  
  cliPkgs = with pkgs; [
    fd tldr xdg-utils wget
    nix-prefetch-scripts btop jq
    steam-run unzip ghostscript
    xdragon python311 ripgrep
    pokemon-colorscripts
    pkg-config ffmpeg
    adwaita-icon-theme
  ];
  
  nogamesPkgs = with pkgs; [
    vkd3d gamemode
    bottles steam
  ];
  baconPkgs = with pkgs; [
    vesktop libreoffice-fresh
    telegram-desktop
    transmission_4-gtk
    bruno
  ];

in
{
  imports = [
    ./homeModules
  ];
  
  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
    };
  };

  
  home = {
    inherit username homeDirectory;
    stateVersion = "23.05";
    packages = baconPkgs ++ nogamesPkgs ++ cliPkgs;
    sessionVariables.FLAKE = "/home/jerpo/nixfiles";
  };

  xdg = {
    inherit configHome;
    enable = true;
  };

  lib.meta = {
    configPath = "${config.home.homeDirectory}/nixfiles/";
    mkMutableSymlink = path: config.lib.file.mkOutOfStoreSymlink
      (config.lib.meta.configPath + lib.strings.removePrefix (toString inputs.self) (toString path));
  };

  # from homeModules
  stylixConfig.enable = true;
  stylixConfig.theme = "everforest";

  emacs.enable = true;
  alacritty.enable = true;
  fish.enable = true;
  ranger.enable = true;

  firefox.enable = true;

  programs.home-manager.enable = true;
}
