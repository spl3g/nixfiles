{ pkgs, inputs, ... }:
let 
  username = "jerpo";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";
  
  cliPkgs = with pkgs; [
    fd fh tldr
    wget btop jq
    bat fzf nix-prefetch-scripts
    steam-run unzip ghostscript
    xdragon python311 ripgrep
    gcc pkg-config
  ];
  gamingPkgs = with pkgs; [
    vkd3d mangohud gamemode
    bottles steam
  ];
  guiPkgs = with pkgs; [
    vesktop libreoffice-fresh
    pokemon-colorscripts
    telegram-desktop
    transmission-gtk
    osu-lazer-bin
    brave bruno
  ];

  imports = builtins.concatMap import [ ./programs ] ++ [ ./themes ];
in
{
  inherit imports;
  
  home = {
    inherit username homeDirectory;
    stateVersion = "23.05";
    packages = guiPkgs ++ gamingPkgs ++ cliPkgs;
  };

  xdg = {
    inherit configHome;
    enable = true;
  };

  programs.home-manager.enable = true;
}
