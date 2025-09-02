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
  # greetd.enable = true;
  ly.enable = true;
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

  virtualisation.containers.enable = true;
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    dockerSocket.enable = true;
    defaultNetwork.settings.dns_enabled = true;
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
    docker-compose
  ];

  services.v2raya = {
    enable = true;
    cliPackage = pkgs.xray;
  };

  fonts = {
    packages = with pkgs; [
      # icon fonts
      material-design-icons

      # normal fonts
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      rubik

      # code font
      nerd-fonts.fira-code
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
  xdg.mime.defaultApplications = {
    "application/pdf" = "firefox.desktop";
  };
  
  programs.dconf.enable = true;
  i18n.defaultLocale = "en_US.UTF-8";
}
