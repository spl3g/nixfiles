{
  pkgs,
  inputs,
  outputs,
  config,
  lib,
  ...
}: let
  username = "jerpo";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";

  cliPkgs = with pkgs; [
    fd
    jq
    zip
    wget
    btop
    unzip
    p7zip
    ffmpeg
    flatpak
    ripgrep
    xdragon
    tealdeer
    steam-run
    python311
    xdg-utils
    pkg-config
    ghostscript
    adwaita-icon-theme
    pokemon-colorscripts
    nix-prefetch-scripts
  ];

  nogamesPkgs = with pkgs; [
    vkd3d
    steam
    bottles
    open-sans
    osu-lazer-bin
  ];
  baconPkgs = with pkgs; [
    vesktop
    telegram-desktop
    libreoffice-fresh
    transmission_4-gtk
  ];

  kube = with pkgs; [
    kubectl
    kubernetes-helm
  ];
in {
  imports = [
    ./homeModules
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.truly-unstable-packages
    ];
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    inherit username homeDirectory;
    stateVersion = "23.05";
    packages = baconPkgs ++ nogamesPkgs ++ cliPkgs ++ kube;
    sessionVariables = {
      NH_FLAKE = "/home/jerpo/nixfiles";
    };
  };

  xdg = {
    inherit configHome;
    enable = true;
  };

  lib.meta = {
    configPath = "${config.home.homeDirectory}/nixfiles/";
    mkMutableSymlink = path:
      config.lib.file.mkOutOfStoreSymlink
      (config.lib.meta.configPath + lib.strings.removePrefix (toString inputs.self) (toString path));
  };

  # from homeModules
  stylixConfig.enable = true;
  stylixConfig.theme = "everforest";

  emacs.enable = true;
  emacs.package = pkgs.emacs30;
  alacritty.enable = true;
  fish.enable = true;

  zen-browser.enable = true;

  tmux.enable = true;

  programs.nix-index-database.comma.enable = true;
  programs.nix-index = {
    enable = true;
  };

  programs.home-manager.enable = true;
}
