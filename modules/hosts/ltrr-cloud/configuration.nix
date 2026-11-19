{
  inputs,
  self,
  ...
}: {
  flake.nixosConfigurations.ltrr-cloud = inputs.nixpkgs-stable.lib.nixosSystem {
    modules = [
      self.nixosModules.ltrr-cloud
    ];
  };

  flake.nixosModules.ltrr-cloud = {
    modulesPath,
    config,
    lib,
    pkgs,
    ...
  }: let
    domain = "kcu.su";
  in {
    imports = [
      (modulesPath + "/installer/scan/not-detected.nix")
      (modulesPath + "/profiles/qemu-guest.nix")
      "${inputs.nixpkgs}/nixos/modules/services/networking/headscale.nix"

      self.nixosModules.nginxProxy
      self.nixosModules.directories

      inputs.disko.nixosModules.disko
      self.diskoConfigurations.ltrr-cloud

      inputs.agenix.nixosModules.default
      inputs.agenix-rekey.nixosModules.default

      # self.nixosModules.omnisearch
    ];
    nixpkgs.hostPlatform = "x86_64-linux";
    nix.settings.experimental-features = ["nix-command" "flakes"];

    disabledModules = ["services/networking/headscale.nix"];

    nixpkgs.overlays = [
      self.overlays.unstable-packages
    ];

    boot.loader.grub = {
      efiSupport = true;
      efiInstallAsRemovable = true;
    };

    networking = {
      interfaces.ens3 = {
        ipv4.addresses = [
          {
            address = "82.108.206.49";
            prefixLength = 32;
          }
        ];
      };
      defaultGateway = {
        address = "82.108.206.1";
        interface = "ens3";
      };
    };

    services.tailscale.enable = true;

    networking.nameservers = ["8.8.8.8" "1.1.1.1"];

    networking.useDHCP = lib.mkDefault false;

    networking.hostName = "ltrr-cloud";

    services.openssh = {
      enable = true;
    };

    environment.systemPackages = map lib.lowPrio [
      pkgs.curl
      pkgs.gitMinimal
    ];

    users.users = {
      root = {
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDJ8UW1BXDGDmlaiARO3a9boTG8wknUyITMz0Z0OJpHx spleefer6@yandex.ru"
        ];
      };
    };

    swapDevices = [
      {
        device = "/var/lib/swapfile";
        size = 1024;
      }
    ];

    networking.nat = {
      enable = false;
      externalInterface = "ens3";
      internalInterfaces = ["wg0"];
    };

    age.secrets.wg-priv-key = {
      rekeyFile = ./secrets/wg-priv.key.age;
    };
    networking.wg-quick = {
      interfaces.wg0 = {
        address = ["10.1.1.1/32"];
        listenPort = 51820;

        privateKeyFile = config.age.secrets.wg-priv-key.path;

        preUp = ''
          sysctl -w net.ipv4.ip_forward=1
          iptables -t nat -A POSTROUTING -s 10.1.1.3/32 -o ens3 -j MASQUERADE
        '';

        postDown = ''
          sysctl -w net.ipv4.ip_forward=0
          iptables -t nat -D POSTROUTING -s 10.1.1.3/32 -o ens3 -j MASQUERADE
        '';

        peers = [
          {
            allowedIPs = ["10.1.1.2/32"];
            publicKey = "kzXzxJu1AdcRI5UwtGOrN6WuTZYqJo++PYRrXdOu/lY=";
            persistentKeepalive = 25;
          }
          {
            allowedIPs = ["10.1.1.3/32"];
            publicKey = "hs6RlxKO7UCfTAXUhDlRiMqg3kuwyNaQdp0ABpM5chM=";
            persistentKeepalive = 25;
          }
          {
            allowedIPs = ["10.1.1.4/32"];
            publicKey = "kosSScP+EUjzL9PvjGPREuMusP1IX0GBYbCLvLOmUHg=";
            persistentKeepalive = 25;
          }
        ];
      };
    };

    networking.firewall.allowedTCPPorts = [
      # http
      80
      443

      #mail
      25
      465
      993
      10025

      25565 # minecraft
      50300 # slskd
      6667 # irc
    ];
    networking.firewall.allowedUDPPorts = [
      51820 # wg
    ];

    services.nginx = {
      enable = true;
      streamConfig = let
        serverIp = "10.1.1.4";
      in ''
        # Proxy HTTPS
        server {
            listen 443;
            proxy_pass ${serverIp}:443;
            proxy_protocol on;
        }

        # Proxy HTTP
        server {
            listen 80;
            proxy_pass ${serverIp}:80;
            proxy_protocol on;
        }

        # Proxy SMTP
        server {
            listen 25;
            proxy_pass ${serverIp}:25;
            proxy_protocol on;
        }

        # Proxy IMAPS
        server {
            listen 993;
            proxy_pass ${serverIp}:993;
            proxy_protocol on;
        }

        # Proxy SMTPS
        server {
            listen 465;
            proxy_pass ${serverIp}:465;
            proxy_protocol on;
        }

        # Proxy slsk
        server {
            listen 50300;
            proxy_pass ${serverIp}:50300;
        }

        # Proxy irc
        server {
            listen 6667;
            proxy_pass ${serverIp}:6667;
        }

        server  {
          listen 25565;
          proxy_pass 127.0.0.1:25566;
          proxy_protocol on;
        }
      '';
    };

    services.headscale = {
      enable = true;
      package = pkgs.headscale;
      port = 8768;
      settings = {
        server_url = "https://headscale.${domain}:443";
        dns = {
          base_domain = "ts.net";
          nameservers.global = ["8.8.8.8"];
          magicdns = true;
        };
      };
    };

    virtualisation.oci-containers.backend = "podman";

    system.stateVersion = "24.05";
  };
}
