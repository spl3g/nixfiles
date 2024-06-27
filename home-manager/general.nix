{ pkgs, inputs, outputs, config, lib, ... }:
let 
  username = "jerpo";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  
  cliPkgs = with pkgs; [
    fd fh tldr xdg-utils
    wget btop jq unstable.devenv
    bat fzf nix-prefetch-scripts
    steam-run unzip ghostscript
    xdragon python311 ripgrep
    gcc pkg-config
  ];
  nogamesPkgs = with pkgs; [
    vkd3d mangohud gamemode
    bottles steam
 ];
  baconPkgs = with pkgs; [
    vesktop libreoffice-fresh
    pokemon-colorscripts
    telegram-desktop
    transmission-gtk
    osu-lazer-bin
    brave bruno spmp
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

  programs.home-manager.enable = true;
}