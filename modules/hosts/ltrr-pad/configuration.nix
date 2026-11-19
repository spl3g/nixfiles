{
  inputs,
  self,
  ...
}: {
  flake.nixosConfigurations.ltrr-pad = inputs.nixpkgs-stable.lib.nixosSystem {
    modules = [
      self.nixosModules.ltrr-pad
    ];
  };

  flake.nixosModules.ltrr-pad = {
    modulesPath,
    pkgs,
    config,
    lib,
    ...
  }: let
    domain = "kcu.su";
  in {
    imports = [
      (modulesPath + "/installer/scan/not-detected.nix")
      (modulesPath + "/profiles/qemu-guest.nix")

      self.nixosModules.directories

      inputs.disko.nixosModules.disko
      self.diskoConfigurations.ltrr-pad

      inputs.agenix.nixosModules.default
      inputs.agenix-rekey.nixosModules.default
    ];

    nixpkgs = {
      config.allowUnfree = true;
      overlays = [
        self.overlays.unstable-packages
      ];
    };

    boot.loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    services.openssh = {
      enable = true;
      settings.PasswordAuthentication = true;
    };

    environment.systemPackages = with pkgs; [
      curl
      gitMinimal
    ];

    users.users = {
      root = {
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDJ8UW1BXDGDmlaiARO3a9boTG8wknUyITMz0Z0OJpHx spleefer6@yandex.ru"
        ];
      };
    };

    users.users.files = {
      isNormalUser = true;
      group = "files";
      uid = 1000;
      linger = true;
      extraGroups = [
        "music"
        "images"
      ];
    };

    users.groups.music = {
      gid = 991;
    };

    createPaths."/srv/files" = {
      owner = "files";
      permissions = "0770";
      group = "files";
      subPaths = {
        "music".group = "music";
        "images".group = "images";
      };
    };

    networking.networkmanager.enable = true;

    networking.hostName = "ltrr-pad";
    networking.firewall = {
      allowedTCPPorts = [
        80
        443

        25565 # minecraft
        50300 # slskd
        6667 # irc
      ];
      allowedUDPPorts = [
        51820
      ];
    };

    age.secrets.wg-priv-key = {
      rekeyFile = ./secrets/wg-priv.key.age;
    };

    networking.wg-quick = {
      interfaces.wg0 = {
        address = ["10.1.1.4/32"];
        listenPort = 51820;

        privateKeyFile = config.age.secrets.wg-priv-key.path;

        peers = [
          {
            endpoint = "${domain}:51820";
            publicKey = "1RwEOL8br97Mujhz3fkfYKcxUFNHYAmt5JbWTbR3ihE=";
            allowedIPs = ["10.1.1.1/32"];
            persistentKeepalive = 25;
          }
        ];
      };
    };

    services.tailscale.enable = true;

    virtualisation.oci-containers.backend = "podman";

    users.groups.books = {
      gid = 1001;
    };
    createPaths."/srv/files/books" = {
      owner = "files";
      group = "books";
      permissions = "0770";
      subPaths = {
        "/library" = {};
        "/bookdrop" = {};
      };
    };

    system.stateVersion = "24.05";
  };
}
