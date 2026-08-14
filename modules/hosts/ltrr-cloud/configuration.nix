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
    ];
    networking.firewall.allowedUDPPorts = [
      51820 # wg
    ];

    environment.etc = {
      "fail2ban/filter.d/authelia.conf".text = ''
         # Fail2Ban filter for Authelia

         # Make sure that the HTTP header "X-Forwarded-For" received by Authelia's backend
         # only contains a single IP address (the one from the end-user), and not the proxy chain
         # (it is misleading: usually, this is the purpose of this header).

         # the failregex rule counts every failed 1FA attempt (first line, wrong username or password) and failed 2FA attempt
         # second line) as a failure.
         # the ignoreregex rule ignores debug, info and warning messages as all authentication failures are flagged as errors

         [Definition]
         failregex = ^.*Unsuccessful 1FA authentication attempt by user .*remote_ip="?<HOST>"? stack.*
                     ^.*Unsuccessful (TOTP|Duo|U2F) authentication attempt by user .*remote_ip="?<HOST>"? stack.*

         ignoreregex = ^.*level=debug.*
                       ^.*level=info.*
                       ^.*level=warning.*

        journalmatch = _SYSTEMD_UNIT=authelia-kcu.service + _COMM=authelia
      '';
    };
    services.fail2ban = {
      enable = false;
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
        authelia = ''
          enabled  = true
          port     = http,https
        '';
      };
    };

    security.acme = {
      acceptTerms = true;
      defaults.email = "notspl3g+acme@duck.com";
    };

    nginxProxy = {
      enable = false;
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

        "search".proxyPass = "http://127.0.0.1:8087";
        "tg".proxyPass = "http://127.0.0.1:8080";
      };

      extraVirtualHosts = {
        "${domain}" = {
          forceSSL = true;
          enableACME = true;
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
        virtualHosts = homeConfig.nginxProxy.extraVirtualHosts;
        url = "http://10.1.1.2";
      };
    };

    services.nginx = {
      streamConfig = ''
        # Proxy SMTP
        server {
            listen 25;
            proxy_pass 10.1.1.2:25;
            proxy_protocol on;
        }

        # Proxy IMAPS
        server {
            listen 993;
            proxy_pass 10.1.1.2:993;
            proxy_protocol on;
        }

        # Proxy SMTPS
        server {
            listen 465;
            proxy_pass 10.1.1.2:465;
            proxy_protocol on;
        }

        server  {
          listen 25565;
          proxy_pass 127.0.0.1:25566;
          proxy_protocol on;
        }
      '';
    };

    # age.secrets.authelia-jwt = {
    #   rekeyFile = ./secrets/authelia-jwt.key.age;
    #   owner = "authelia-kcu";
    #   group = "authelia-kcu";
    # };
    # age.secrets.authelia-storage = {
    #   rekeyFile = ./secrets/authelia-storage.key.age;
    #   owner = "authelia-kcu";
    #   group = "authelia-kcu";
    # };
    # age.secrets.authelia-users = {
    #   rekeyFile = ./secrets/authelia-users.yaml.age;
    #   owner = "authelia-kcu";
    #   group = "authelia-kcu";
    # };
    # services.authelia.instances.kcu = {
    #   enable = true;
    #   secrets = {
    #     jwtSecretFile = config.age.secrets.authelia-jwt.path;
    #     storageEncryptionKeyFile = config.age.secrets.authelia-storage.path;
    #   };
    #   settings = {
    #     authentication_backend = {
    #       file = {
    #         path = config.age.secrets.authelia-users.path;
    #       };
    #     };

    #     storage = {
    #       local = {};
    #     };

    #     access_control = {
    #       default_policy = "deny";
    #       rules = [
    #         {
    #           domain = ["auth.${domain}"];
    #           policy = "bypass";
    #         }
    #         {
    #           domain = ["git.${domain}"];
    #           policy = "bypass";
    #         }
    #         {
    #           domain = ["tg.${domain}"];
    #           policy = "bypass";
    #         }
    #         {
    #           domain = ["books.${domain}"];
    #           policy = "bypass";
    #           resources = [
    #             "^/api/v1/opds.*"
    #             "^/api/koreader.*"
    #           ];
    #         }
    #         {
    #           domain = ["*.${domain}"];
    #           policy = "one_factor";
    #         }
    #       ];
    #     };

    #     session = {
    #       name = "authelia_session";
    #       expiration = "12h";
    #       inactivity = "45m";
    #       cookies = [
    #         {
    #           inherit domain;
    #           authelia_url = "https://auth.kcu.su";
    #         }
    #       ];
    #     };

    #     server.endpoints.authz.auth-request = {
    #       implementation = "AuthRequest";
    #       authn_strategies = [
    #         {
    #           name = "CookieSession";
    #         }
    #       ];
    #     };

    #     storage = {
    #       local = {
    #         path = "/var/lib/authelia-kcu/db.sqlite3";
    #       };
    #     };

    #     notifier = {
    #       disable_startup_check = false;
    #       filesystem = {
    #         filename = "/var/lib/authelia-kcu/notification.txt";
    #       };
    #     };
    #   };
    # };

    # services.omnisearch = {
    #   enable = true;
    #   settings = {
    #     server = {
    #       host = "0.0.0.0";
    #       port = 8087;
    #       domain = "https://search.${domain}";
    #     };
    #     engines = {
    #       engines = "*,-mojeek";
    #     };
    #   };
    # };

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
    # virtualisation.oci-containers.containers = {
    #   "uptime-kuma" = {
    #     image = "louislam/uptime-kuma:2";
    #     volumes = [
    #       "/var/lib/uptime-kuma:/app/data"
    #     ];
    #     ports = [
    #       "127.0.0.1:8762:3001"
    #     ];
    #     capabilities = {
    #       NET_RAW = true;
    #     };
    #   };
    # };

    # services.beszel.hub = {
    #   enable = true;
    # };

    services.postfix = {
      enable = true;
      enableSmtp = false;
      settings = {
        main = {
          myhostname = "mail.${domain}";
          mydomain = domain;
          myorigin = "$mydomain";

          mynetworks = ["127.0.0.0/8" "[::1]/128" "10.1.1.0/24"];

          mydestination = "localhost";
          local_recipient_maps = "";

          relay_domains = [domain];
          smtpd_relay_restrictions = "permit_mynetworks, reject_unauth_destination";
          smtpd_client_restrictions = "permit_mynetworks,reject";
        };
        master = {
          "10025" = {
            command = "smtpd";
            type = "inet";
            chroot = true;
            private = false;
            args = [
              "-o"
              "syslog_name=postfix/wireguard-relay"
            ];
          };
          "smtp" = {};
        };
      };
    };

    services.rustdesk-server = {
      enable = true;
      openFirewall = true;
      signal.relayHosts = ["${domain}"];
    };

    system.stateVersion = "24.05";
  };
}
