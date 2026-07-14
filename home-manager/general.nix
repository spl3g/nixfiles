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
    ripgrep

    jq
    wget
    btop
    tealdeer

    zip
    unzip
    p7zip

    ffmpeg
    ghostscript
    dragon-drop
    steam-run
    python312
    pokemon-colorscripts

    xdg-utils
    pkg-config
    nix-prefetch-scripts
    adwaita-icon-theme
    postgresql
  ];

  freesm-launcher = inputs.freesm.packages.${pkgs.stdenv.hostPlatform.system}.freesmlauncher;
  nogamesPkgs = with pkgs; [
    vkd3d
    steam
    faugus-launcher
    open-sans
    osu-lazer-bin
    freesm-launcher
  ];
  baconPkgs = with pkgs; [
    stable.vesktop
    chromium
    geeqie
    darktable
    telegram-desktop
    onlyoffice-desktopeditors
    transmission_4-gtk
    blender
    gelly
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
      outputs.overlays.stable-packages
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
