{
  modulesPath,
  lib,
  pkgs,
  config,
  ...
}: let
  domain = "kcu.su";
in {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ./disk-config.nix
    ../serverModules/nginx.nix
    ../serverModules/files.nix
    ../serverModules/gonic.nix
  ];

  nixpkgs.config.allowUnfree = true;
  
  sops = {
    defaultSopsFile = ../../secrets/ltrr-home/secrets.yaml;
    defaultSopsFormat = "yaml";
    age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  };

  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  services.openssh.enable = true;

  environment.systemPackages = with pkgs; [
    curl
    gitMinimal
    beets
  ];

  users.users = {
    root = {
      openssh.authorizedKeys.keys =
        [
          # change this to your ssh key
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDJ8UW1BXDGDmlaiARO3a9boTG8wknUyITMz0Z0OJpHx spleefer6@yandex.ru"
        ];
      hashedPassword = "$y$j9T$v3n61T5.hOGZUgzeHKOp41$qli1X0.ewVopbLcMrqUX/rKggtvsYAKz2VwsSE/7pAA";
    };
  };

  filesDir = {
    enable = true;
    subPaths = [
      {
        path = "music";
        group = "music";
      }
      {
        path = "images";
        group = "images";
      }
    ];
  };

  networking.hostName = "ltrr-home";
  networking.firewall = {
    allowedTCPPorts = [80 5030 2049];
    allowedUDPPorts = [51820];
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "notspl3g+acme@duck.com";
  };

  nginx = {
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
    };
  };

  sops.secrets.wg-private-key = {
    restartUnits = ["wg-quick-wg0.service"];
  };
  networking.wg-quick = {
    interfaces.wg0 = {
      address = ["10.1.1.2/32"];
      listenPort = 51820;

      privateKeyFile = config.sops.secrets.wg-private-key.path;

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
    "d /srv/files/slskd 0740 slskd music"
    "d /opt/traggo/data"
    "d /var/lib/traggo"
  ];
  users.users.slskd.extraGroups = ["files"];
  services.slskd = {
    enable = true;
    environmentFile = "/var/lib/slskd/env";
    group = "music";
    settings = {
      shares.directories = ["/srv/files/music"];
      directories.downloads = "/srv/files/slskd";
    };
    openFirewall = true;
    domain = null;
  };

  sops.secrets.navidrome-env = {
    restartUnits = ["navidrome.service"];
  };
  users.users.navidrome.extraGroups = ["files" "music"];
  services.navidrome = {
    enable = true;
    settings = {
      BaseUrl = "https://navidrome.${domain}";
      MusicFolder = "/srv/files/music";
      PlaylistsPath = "playlists";
      Scanner.PurgeMissing = "always";
      EnableSharing = true;
    };
    environmentFile = config.sops.secrets.navidrome-env.path;
  };

  virtualisation.oci-containers.backend = "podman";
  virtualisation.oci-containers.containers = {
    aonsoku = {
      image = "ghcr.io/victoralvesf/aonsoku:latest";
      ports = [
        "127.0.0.1:5692:8080"
      ];
      environment = {
        SERVER_URL = "https://navidrome.${domain}";
        HIDE_SERVER = "true";
      };
    };

    traggo = {
      image = "traggo/server";
      ports = [
        "127.0.0.1:7093:3030"
      ];
      environmentFiles = ["/var/lib/traggo/env"];
      workdir = "/opt/traggo/";
      volumes = [
        "/opt/traggo/data:/opt/traggo/data"
      ];
    };
  };

  services.immich = {
    enable = true;
  };

  system.stateVersion = "24.05";
}
