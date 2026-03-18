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
    ];
    nixpkgs.hostPlatform = "x86_64-linux";

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
            address = "194.156.117.206";
            prefixLength = 32;
          }
        ];
      };
      defaultGateway = {
        address = "100.100.1.1";
        interface = "ens3";
      };
    };

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
        size = 2 * 1024;
      }
    ];

    networking.nat = {
      enable = true;
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
          # 16261
          iptables -t nat -I PREROUTING 1 -i ens3 -p udp --dport 16261 -j DNAT --to-destination 10.1.1.2:16261
          iptables -A FORWARD -p udp -d 10.1.1.2 --dport 16261 -j ACCEPT
          iptables -t nat -A POSTROUTING -o wg0 -p udp --dport 16261 -d 10.1.1.2 -j MASQUERADE

          # 16262
          iptables -t nat -I PREROUTING 1 -i ens3 -p udp --dport 16262 -j DNAT --to-destination 10.1.1.2:16262
          iptables -A FORWARD -p udp -d 10.1.1.2 --dport 16262 -j ACCEPT
          iptables -t nat -A POSTROUTING -o wg0 -p udp --dport 16262 -d 10.1.1.2 -j MASQUERADE
        '';
        postDown = ''
          # 16261
          iptables -t nat -D PREROUTING -i ens3 -p udp --dport 16261 -j DNAT --to-destination 10.1.1.2:16261
          iptables -D FORWARD -p udp -d 10.1.1.2 --dport 16261 -j ACCEPT
          iptables -t nat -D POSTROUTING -o wg0 -p udp --dport 16261 -d 10.1.1.2 -j MASQUERADE

          # 16262
          iptables -t nat -D PREROUTING -i ens3 -p udp --dport 16262 -j DNAT --to-destination 10.1.1.2:16262
          iptables -D FORWARD -p udp -d 10.1.1.2 --dport 16262 -j ACCEPT
          iptables -t nat -D POSTROUTING -o wg0 -p udp --dport 16262 -d 10.1.1.2 -j MASQUERADE
        '';

        peers = [
          {
            allowedIPs = ["10.1.1.2/32"];
            publicKey = "kzXzxJu1AdcRI5UwtGOrN6WuTZYqJo++PYRrXdOu/lY=";
            persistentKeepalive = 25;
          }
        ];
      };
    };

    networking.firewall.allowedTCPPorts = [80 443 25565];
    networking.firewall.allowedUDPPorts = [51820 16261 16262];

    services.fail2ban = {
      enable = true;
      ignoreIP = [
        "10.0.0.0/8"
      ];
      bantime = "24h";
      bantime-increment = {
        enable = true;
        multipliers = "1 2 4 8 16 32 64";
        maxtime = "168h";
        overalljails = true;
      };

      jails = {
        nginx-http-auth.settings = {
          enabled = true;
          port = "http,https";
          logpath = "/var/log/nginx/*.log";
          backend = "auto";
        };
        nginx-botsearch.settings = {
          enabled = true;
          port = "http,https";
          logpath = "/var/log/nginx/*.log";
          backend = "auto";
        };
        nginx-bad-request.settings = {
          enabled = true;
          port = "http,https";
          logpath = "/var/log/nginx/*.log";
          backend = "auto";
        };
      };
    };

    security.acme = {
      acceptTerms = true;
      defaults.email = "notspl3g+acme@duck.com";
    };

    nginxProxy = {
      enable = true;
      acme.enable = true;

      inherit domain;
      subdomains = {
        "headscale" = {
          proxyPass = "http://127.0.0.1:8768";
          proxyWebsockets = true;
          recommendedProxySettings = true;
        };
        "uptime" = {
          proxyPass = "http://127.0.0.1:8762";
          proxyWebsockets = true;
          recommendedProxySettings = true;
        };
        "monitor" = {
          proxyPass = "http://127.0.0.1:8090";
        };
        "auth".proxyPass = "http://127.0.0.1:9091";
      };

      extraVirtualHosts = {
        "kcu.su" = {
          forceSSL = true;
          enableACME = true;
          locations."/apple" = {
            root = "/var/www";
          };
          locations."/" = {
            return = 444;
          };
        };

        "_".locations = {
          "/" = {
            return = 444;
          };
        };
      };

      home = let
        homeConfig = self.nixosConfigurations.ltrr-block.config;
      in {
        subdomains = homeConfig.nginxProxy.subdomains;
        url = "http://10.1.1.2";
      };
    };

    age.secrets.authelia-jwt = {
      rekeyFile = ./secrets/authelia-jwt.key.age;
      owner = "authelia-kcu";
      group = "authelia-kcu";
    };
    age.secrets.authelia-storage = {
      rekeyFile = ./secrets/authelia-storage.key.age;
      owner = "authelia-kcu";
      group = "authelia-kcu";
    };
    services.authelia.instances.kcu = {
      enable = true;
      secrets = {
        jwtSecretFile = config.age.secrets.authelia-jwt.path;
        storageEncryptionKeyFile = config.age.secrets.authelia-storage.path;
      };
      settings = {
        authentication_backend = {
          file = {
            path = "/var/lib/authelia-kcu/users_database.yml";
          };
        };

        storage = {
          local = {};
        };

        access_control = {
          default_policy = "deny";
          rules = [
            {
              domain = ["auth.${domain}"];
              policy = "bypass";
            }
            {
              domain = ["*.${domain}"];
              policy = "one_factor";
            }
          ];
        };

        session = {
          name = "authelia_session";
          expiration = "12h";
          inactivity = "45m";
          cookies = [
            {
              inherit domain;
              authelia_url = "https://auth.kcu.su";
            }
          ];
        };

        storage = {
          local = {
            path = "/var/lib/authelia-kcu/db.sqlite3";
          };
        };

        notifier = {
          disable_startup_check = false;
          filesystem = {
            filename = "/var/lib/authelia-kcu/notification.txt";
          };
        };
      };
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

    createPaths = {
      "/var/lib/uptime-kuma" = {
        owner = "root";
        group = "root";
      };
    };
    virtualisation.oci-containers.backend = "podman";
    virtualisation.oci-containers.containers = {
      "uptime-kuma" = {
        image = "louislam/uptime-kuma:2";
        volumes = [
          "/var/lib/uptime-kuma:/app/data"
        ];
        ports = [
          "127.0.0.1:8762:3001"
        ];
        capabilities = {
          NET_RAW = true;
        };
      };
    };

    services.beszel.hub = {
      enable = true;
    };

    system.stateVersion = "24.05";
  };
}
