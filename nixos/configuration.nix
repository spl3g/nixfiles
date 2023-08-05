# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, inputs, outputs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      inputs.home-manager.nixosModules.home-manager
    ];

  home-manager = {
    extraSpecialArgs = { inherit inputs outputs; };
    useGlobalPkgs = true;
    useUserPackages = true;
    users.jerpo = import ../home-manager/home/home.nix;
  };
   
  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "ltrr"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Asia/Yekaterinburg";

  # Select internationalisation properties.
  i18n.defaultLocale = "ru_RU.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "ru_RU.UTF-8";
    LC_IDENTIFICATION = "ru_RU.UTF-8";
    LC_MEASUREMENT = "ru_RU.UTF-8";
    LC_MONETARY = "ru_RU.UTF-8";
    LC_NAME = "ru_RU.UTF-8";
    LC_NUMERIC = "ru_RU.UTF-8";
    LC_PAPER = "ru_RU.UTF-8";
    LC_TELEPHONE = "ru_RU.UTF-8";
    LC_TIME = "ru_RU.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    displayManager.sddm = {
      enable = true;
      theme = "sugar-dark";
    };
    # displayManager.startx.enable = true;
    displayManager.defaultSession = "none+bspwm";
    desktopManager.gnome.enable = true;
    windowManager.dwm.enable = true;
    windowManager.bspwm.enable = true;
  };
  # nixpkgs.overlays = [
  #   (final: prev: {
  #     dwm = prev.dwm.overrideAttrs (oldAttrs: rec {
  #       patches = [
  #         # Fibonacci for indle
  #         (prev.fetchpatch {
  #            url = "https://dwm.suckless.org/patches/fibonacci/dwm-fibonacci-6.2.diff";
  #            sha256 = "12y4kknly5irwd6yhqj1zfr3h06hixi2p7ybjymhhhy0ixr7c49d";
  #          })
  #         # Dwmc for sxhkd
  #         (prev.fetchpatch {
  #            url = "https://dwm.suckless.org/patches/dwmc/dwm-dwmc-6.2.diff";
  #            sha256 = "0k1cyjji7s4mwx7m9wk4kj0ax8yc1q9bpz3rjhkw45dz43r4vp4f";
  #         })
  #         # Attachaside
  #         (prev.fetchpatch {
  #            url = "https://dwm.suckless.org/patches/attachaside/dwm-attachaside-6.3.diff";
  #            sha256 = "10kg93gvmpcjlx35624bhcvdpv8q8vkqpmckax2n0vr4bvrg4ipd";
  #         })
  #       ];
  #       configFile = pkgs.writeText "config.def.h" (builtins.readFile ./dwm/config.h);
  #       postPatch = "${oldAttrs.postPatch}\n cp ${configFile} config.def.h";
  #     });
  #   })
  # ];

  # Configure keymap in X11
  services.xserver = {
    layout = "us,ru";
    xkbOptions = "grp:win_space_toggle";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  programs.fish.enable = true;
  users.defaultUserShell = pkgs.fish;
  users.users.jerpo = {
    isNormalUser = true;
    description = "jerpo";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nix.settings.auto-optimise-store = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    wget
    git
    firefox
    neovim
    ripgrep
    fd
    killall
    fzf
    silver-searcher
    tldr
    unzip
    cmake
    python3Full
    nodejs_20
    xorg.xinit
    gnumake
    gcc
    btrfs-progs
    ntfs3g
    libtool
    jq
    vkd3d
    dxvk
    wineWowPackages.full
    wineWowPackages.fonts
    winetricks
    nvtop-nvidia
    nix-prefetch-scripts
    xkb-switch
    pamixer
    rustup
  ];

  services.v2raya.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
