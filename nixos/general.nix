{ inputs, outputs, lib, config, pkgs, ... }: {
  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
      outputs.overlays.emacs-overlay

      # You can also add overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default
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
    };
  };
  networking.networkmanager = {
    enable = true;
  };
  networking.firewall.enable = false;
  networking.nftables.enable = true;
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };
  security.rtkit.enable = true;
  services.pipewire = {
      enable = true;
      audio.enable = true;
      wireplumber.enable = true;
      alsa.enable = true;
      pulse.enable = true;
      jack.enable = true;
  };
  environment.systemPackages = with pkgs; [
    vim
    git 
  ];
  services.udisks2 = {
    enable = true;
    mountOnMedia = true;
  };
  programs.dconf.enable = true;
  i18n.defaultLocale = "ru_RU.UTF-8";
  programs.fish.enable = true;
  users.users = {
    jerpo = {
      isNormalUser = true;
      shell = pkgs.fish;
      extraGroups = [ "networkmanager" "wheel" "docker" "libvirtd" "input" "adbusers" ];
    };
  };

}
