{ inputs, outputs, lib, config, pkgs, ... }: {
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/nixos):
    # outputs.nixosModules.example

    # Or modules from other flakes (such as nixos-hardware):
    # inputs.hardware.nixosModules.common-cpu-amd
    # inputs.hardware.nixosModules.common-ssd

    ./hardware-configuration.nix
    inputs.home-manager.nixosModules.home-manager
  ];

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
      options = "--delete-older-than 1w";
    };

    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
      # Enable cachix
      substituters = [ "https://nix-gaming.cachix.org" ];
      trusted-public-keys = [ "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4=" ];

    };
  };

  time.timeZone = "Europe/Moscow";
  networking.hostName = "ltrr";
  networking.networkmanager.enable = true;

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  services = {
    xserver = {
      enable = true;
      
      # dm
      displayManager = {
        lightdm.enable = false;
      #   defaultSession = "none+bspwm";
      #   sddm = {
      #     enable = true;
      #     theme = "sugar-dark";
      #   };
      };
      
      # libinput
      libinput.enable = true;

      # wm
      windowManager.bspwm.enable = true;
      
      # Layout
      layout = "us,ru";
      xkbOptions = "grp:win_space_toggle";
    };

    acpid = {
      enable = true;
      powerEventCommands = "";
      lidEventCommands = "firefox";
    };

    # Printing
    # printing.enable = true;
    # printing.drivers = [ pkgs.hplipWithPlugin ];
    # avahi = {
    #   enable = true;
    #   nssmdns = true;
    #   openFirewall = true;
    # };
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

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "jerpo" ];

  environment.systemPackages = with pkgs; [
    vim
    git 
    btrfs-progs
    ntfs3g
    xorg.xinit
    carapace
  ];
  services.udisks2 = {
    enable = true;
    mountOnMedia = true;
  };

  programs.dconf.enable = true;
  i18n.defaultLocale = "ru_RU.UTF-8";

  programs.adb.enable = true;
  services.udev.packages = [
    pkgs.android-udev-rules
  ];

  programs.fish.enable = true;
  users.users = {
    jerpo = {
      isNormalUser = true;
      shell = pkgs.fish;
      openssh.authorizedKeys.keys = [
        # TODO: Add your SSH public key(s) here, if you plan on using SSH to connect
      ];
      extraGroups = [ "networkmanager" "wheel" "docker" "input" "adbusers" ];
    };
  };

  home-manager = {
    extraSpecialArgs = { inherit inputs outputs; };
    useGlobalPkgs = true;
    useUserPackages = true;
    users.jerpo = import ../home-manager/home.nix;
  };

  qt = {
    platformTheme = "gtk";
    style = "gtk";
  };

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.05";
}
