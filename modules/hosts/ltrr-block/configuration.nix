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

    users.users.mc = {
      isNormalUser = true;
      packages = with pkgs; [
        steamcmd
        tmux
        steam-run
      ];
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
      allowedTCPPorts = [80 5030 2049 25565];
      allowedUDPPorts = [51820 16261 16262];
    };

    security.acme = {
      acceptTerms = true;
      defaults.email = "notspl3g+acme@duck.com";
    };

    nginxProxy = {
      enable = true;
      domain = domain;

      recommendedProxySettings = false;
      subdomains = {
        "slskd" = {
          proxyPass = "http://127.0.0.1:${toString config.services.slskd.settings.web.port}";
          proxyWebsockets = true;
        };
        "immich" = {
          proxyPass = "http://localhost:${toString config.services.immich.port}";
          proxyWebsockets = true;
          recommendedProxySettings = true;
          extraConfig = ''
            client_max_body_size 50000M;
            proxy_read_timeout   600s;
            proxy_send_timeout   600s;
            send_timeout         600s;
          '';
        };

        "music".proxyPass = "http://127.0.0.1:5692";
        "navidrome".proxyPass = "http://127.0.0.1:4533";
        "files".proxyPass = "http://127.0.0.1:${toString config.services.filebrowser.settings.port}";
        "track".proxyPass = "http://127.0.0.1:7093";
        "tube".proxyPass = "http://127.0.0.1:5410";
        "torrents".proxyPass = "http://127.0.0.1:7317";
        "jellyfin".proxyPass = "http://127.0.0.1:8096";
        "lidarr" = {
          proxyPass = "http://127.0.0.1:8686";
          proxyWebsockets = true;
        };

        "prowlarr".proxyPass = "http://127.0.0.1:9696";
        "shelfmark".proxyPass = "http://127.0.0.1:8084";
      };
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

    users.users.filebrowser.extraGroups = ["music" "images"];
    systemd.services.filebrowser.serviceConfig.SupplementaryGroups = ["music" "images"];
    services.filebrowser = {
      enable = true;
      group = "files";
      settings = {
        root = "/srv/files";
        port = 9337;
      };
    };

    systemd.tmpfiles.rules = [
      "d /srv/files/slskd 0770 slskd music"
      "d /opt/traggo/data"
      "d /var/lib/traggo"
    ];
    users.users.slskd.extraGroups = ["files"];
    age.secrets.slskd-env = {
      rekeyFile = ./secrets/slskd.env.age;
    };
    systemd.services.slskd.serviceConfig.UMask = 0002;
    services.slskd = {
      enable = true;
      environmentFile = config.age.secrets.slskd-env.path;
      group = "music";
      settings = {
        shares = {
          directories = ["/srv/files/music"];
          filters = ["explo"];
        };
        directories.downloads = "/srv/files/slskd";
        permissions.file.mode = 770;
        web.authentication.api_keys = {
          arr = {
            key = "mahChufaeweipemeeheexaoDahchohZi";
            cidr = "127.0.0.1/32";
          };
        };
      };
      openFirewall = true;
      domain = null;
    };

    users.users.jellyfin.extraGroups = ["files"];
    services.jellyfin = {
      enable = true;
    };

    createPaths."/srv/files/torrents" = {
      owner = "files";
      group = "files";
      permissions = "0770";
      subPaths = {
        "incomplete" = {};
        "completed" = {};
      };
    };

    users.users.lidarr.extraGroups = ["files"];
    services.lidarr = {
      enable = true;
      group = "music";
    };

    services.prowlarr = {
      enable = true;
    };

    services.qbittorrent = {
      enable = true;
      user = "files";
      group = "files";
      webuiPort = 7317;
      serverConfig = {
        Preferences = {
          WebUI = {
            Enabled = "true";
            Username = "spl3g";
            Password_PBKDF2 = "@ByteArray(drq4VxxcJLLK0Bma9mxUeg==:47J+BVdVlmpJt0Hb9LqiAj6rTK3ZlFPvy00PdGPtmeXO7SslNg/4Uej7Vmwn3+oFPuE1q/9tm1z1UogfopREUQ==)";
          };
          Downloads = {
            SavePath = "/srv/files/torrents/complete/";
            TempPathEnabled = "true";
            TempPath = "/srv/files/torrents/incomplete/";
          };
        };
      };
    };

    createPaths."/var/lib/soularr" = {
      owner = "slskd";
      group = "slskd";
      permissions = "0770";
    };
    age.secrets.soularr-config = {
      rekeyFile = ./secrets/soularr.conf.age;
      owner = "slskd";
      group = "music";
    };
    virtualisation.oci-containers.containers.soularr = {
      image = "mrusse08/soularr:latest";
      environment = {
        TZ = "Asia/Yekaterinburg";
        SCRIPT_INTERVAL = "300";
      };
      # slskd:music
      user = "991:991";
      volumes = [
        "/srv/files/slskd:/downloads"
        "/var/lib/soularr:/data"
        "${config.age.secrets.soularr-config.path}:/data/config.ini"
      ];
      networks = [
        "host"
      ];
    };

    createPaths."/srv/files/explo" = {
      owner = "files";
      group = "music";
      permissions = "0770";
    };
    age.secrets.explo-env = {
      rekeyFile = ./secrets/explo.env.age;
      owner = "files";
      group = "music";
    };
    # TODO: write a module for explo
    virtualisation.oci-containers.containers.explo = {
      image = "ghcr.io/lumepart/explo:latest";
      volumes = [
        "${config.age.secrets.explo-env.path}:/opt/explo/.env"
        "/srv/files/explo:/data/"
        "/srv/files/slskd:/slskd/"
      ];
      environment = {
        EXECUTE_ON_START = "true";
      };
      networks = [
        "host"
      ];
      # files:music
      extraOptions = [
        "--uidmap=0:1000:1"
        "--gidmap=0:991:1"
        "--uidmap=1:100000:65535"
        "--gidmap=1:100000:65535"
      ];
    };

    age.secrets.navidrome-env = {
      rekeyFile = ./secrets/navidrome.env.age;
    };
    users.users.navidrome.extraGroups = ["files" "music"];
    systemd.services.navidrome.serviceConfig.BindReadOnlyPaths = ["/srv/files/explo"];
    services.navidrome = {
      enable = true;
      package = pkgs.unstable.navidrome;
      settings = {
        BaseUrl = "https://navidrome.${domain}";
        MusicFolder = "/srv/files/music";
        PlaylistsPath = "playlists";
        Scanner.PurgeMissing = "always";
        EnableSharing = true;
      };
      environmentFile = config.age.secrets.navidrome-env.path;
    };

    age.secrets.xray-config = {
      rekeyFile = ./secrets/xray.json.age;
    };
    services.xray = {
      enable = true;
      settingsFile = config.age.secrets.xray-config.path;
    };

    services.invidious = {
      enable = true;
      address = "127.0.0.1";
      port = 5410;
      domain = "tube.${domain}";
      settings = {
        http_proxy = {
          host = "127.0.0.1";
          port = 10801;
          user = "";
          password = "";
        };
      };
    };

    virtualisation.oci-containers.backend = "podman";
    virtualisation.oci-containers.containers.aonsoku = {
      image = "ghcr.io/victoralvesf/aonsoku:latest";
      ports = [
        "127.0.0.1:5692:8080"
      ];
      environment = {
        SERVER_URL = "https://navidrome.${domain}";
        HIDE_SERVER = "true";
      };
    };

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

    createPaths."/var/lib/cwa" = {
      owner = "files";
      group = "books";
      permissions = "0750";
    };

    # age.secrets.cwa-env = {
    #   rekeyFile = ./secrets/cwa.env.age;
    # };
    # virtualisation.oci-containers.containers.cwa = {
    #   image = "ghcr.io/crocodilestick/calibre-web-automated:latest";
    #   environmentFiles = [
    #     config.age.secrets.cwa-env.path
    #   ];
    #   environment = {
    #     TZ = "Europe/Moscow";
    #     PUID = "1000";
    #     PGID = "1001";
    #   };
    #   ports = [
    #     "127.0.0.1:8083:8083"
    #   ];
    #   volumes = [
    #     "/srv/files/books:/calibre-library"
    #     "/srv/files/books/injest:/cwa-book-ingest"
    #     "/var/lib/cwa:/config"
    #   ];
    # };

    createPaths."/var/lib/shelfmark" = {
      owner = "files";
      group = "books";
      permissions = "0750";
    };

    virtualisation.oci-containers.containers.shelfmark = {
      image = "ghcr.io/calibrain/shelfmark:latest";
      environment = {
        PUID = "1000";
        PGID = "1001";
      };

      ports = [
        "127.0.0.1:8084:8084"
      ];

      volumes = [
        "/srv/files/books/injest:/books"
        "/var/lib/shelfmark:/config"
      ];
      networks = [
        "host"
      ];
    };
    services.booklore = {
      enable = true;
      subdomain = "books";
      uid = "1000";
      gid = "1001";
      settings = {
        timezone = "Europe/Yekaterinburg";
        booksDir = "/srv/files/books/library";
        bookdropDir = "/srv/files/books/injest";
      };
    };

    services.watcharr = {
      enable = true;
      subdomain = "watched";
    };

    services.immich = {
      enable = true;
    };

    system.stateVersion = "24.05";
  };
}
