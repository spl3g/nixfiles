{ inputs, outputs, lib, config, pkgs, ... }:

{
  imports = [
    ./nixosModules
  ];
  
  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
    };
  };

  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    gc = {
      automatic = true;
      dates = "weekly";
    };

    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Deduplicate and optimize nix store
      auto-optimise-store = true;

      trusted-users = [ "root" "@wheel" ];
    };
  };

  # from nixosModules
  disks.enable = true;
  user.enable = true;
  greetd.enable = true;
  stylixConfig.enable = true;
  kanata.enable = true;
  
  networking = {
    networkmanager = {
      enable = true;
    };
    nftables.enable = true;
    hosts = {
      "127.0.0.1" = ["v2raya.local"];
    };

  };

  services.postgresql = {
    enable = true;
  };
  
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };
  
  security.rtkit.enable = true;
  security.polkit.enable = true;
  
  services.pipewire = {
    enable = true;
    audio.enable = true;
    wireplumber.enable = true;
    alsa.enable = true;
    pulse.enable = true;
    jack.enable = true;
  };
  
  environment.systemPackages = with pkgs; [
    neovim
    git
    nh
    home-manager
  ];

  services.v2raya.enable = true;

  fonts = {
    packages = with pkgs; [
      # icon fonts
      material-design-icons

      # normal fonts
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      rubik

      # code font
      (nerdfonts.override {fonts = ["SourceCodePro"];})
    ];

    enableDefaultPackages = false;

    # user defined fonts
    # the reason there's Noto Color Emoji everywhere is to override DejaVu's
    # B&W emojis that would sometimes show instead of some Color emojis
    fontconfig.defaultFonts = {
      serif = [ "Noto Serif" "Noto Color Emoji" ];
      sansSerif = [ "Noto Sans" "Noto Color Emoji" ];
      monospace = [ "Sauce Code Pro Nerd Font" ];
      emoji = [ "Noto Color Emoji" ];
    };
  };

  
  programs.dconf.enable = true;
  i18n.defaultLocale = "en_US.UTF-8";
}
