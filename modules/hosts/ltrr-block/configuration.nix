{
  inputs,
  self,
  ...
}: {
  flake.nixosConfigurations.ltrr-block = inputs.nixpkgs-stable.lib.nixosSystem {
    modules = [
      self.nixosModules.ltrr-block
    ];
  };

  flake.nixosModules.ltrr-block = {
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

      self.nixosModules.nginxProxy
      self.nixosModules.directories
      self.nixosModules.booklore
      self.nixosModules.watcharr

      inputs.disko.nixosModules.disko
      self.diskoConfigurations.ltrr-block

      inputs.agenix.nixosModules.default
      inputs.agenix-rekey.nixosModules.default
    ];

    nixpkgs = {
      config.allowUnfree = true;
      overlays = [
        self.overlays.unstable-packages
      ];
    };

    boot.loader.grub = {
      efiSupport = true;
      efiInstallAsRemovable = true;
    };

    services.openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
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

    networking.hostName = "ltrr-block";
    networking.firewall = {
      allowedTCPPorts = [
        80

        25565 # minecraft
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
        address = ["10.1.1.2/32"];
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
