{
  modulesPath,
  lib,
  pkgs,
  config,
  inputs,
  outputs,
  ...
}: let
  domain = "kcu.su";
in {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ./disk-config.nix
    ../serverModules/nginx.nix
    ../serverModules/directories.nix
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.unstable-packages
    ];
    config.allowUnfree = true;
  };

  sops = {
    defaultSopsFile = ../../secrets/ltrr-home/secrets.yaml;
    defaultSopsFormat = "yaml";
    age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
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
    unstable.beets
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

  networking.hostName = "ltrr-home";
  networking.firewall = {
    allowedTCPPorts = [80 5030 2049 25565];
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
      "tube".proxyPass = "http://127.0.0.1:5410";

      "lidarr" = {
        proxyPass = "http://127.0.0.1:8686";
        proxyWebsockets = true;
      };
      "prowlarr".proxyPass = "http://127.0.0.1:9696";
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
    "d /srv/files/slskd 0770 slskd music"
    "d /opt/traggo/data"
    "d /var/lib/traggo"
  ];
  users.users.slskd.extraGroups = ["files"];
  sops.secrets.slskd-env = {
    restartUnits = ["slskd.service"];
  };
  systemd.services.slskd.serviceConfig.UMask = 0002;
  services.slskd = {
    enable = true;
    environmentFile = config.sops.secrets.slskd-env.path;
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

  users.users.lidarr.extraGroups = ["files"];
  services.lidarr = {
    enable = true;
    group = "music";
  };

  services.prowlarr = {
    enable = true;
  };

  createPaths."/var/lib/soularr" = {
    owner = "slskd";
    group = "slskd";
    permissions = "0770";
  };
  sops.secrets.soularr-config = {
    owner = "slskd";
    group = "music";
    restartUnits = ["podman-soularr.service"];
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
      "${config.sops.secrets.soularr-config.path}:/data/config.ini"
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
  sops.secrets.explo-env = {
    owner = "files";
    group = "music";
    restartUnits = ["podman-explo.service"];
  };
  # TODO: write a module for explo
  virtualisation.oci-containers.containers.explo = {
    image = "ghcr.io/lumepart/explo:latest";
    volumes = [
      "${config.sops.secrets.explo-env.path}:/opt/explo/.env"
      "/srv/files/explo:/data/"
      "/srv/files/slskd:/slskd/"
    ];
    environment = {
      # EXECUTE_ON_START = "true";
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

  sops.secrets.navidrome-env = {
    restartUnits = ["navidrome.service"];
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
    environmentFile = config.sops.secrets.navidrome-env.path;
  };

  sops.secrets.xray-config = {
    restartUnits = ["xray.service"];
  };
  services.xray = {
    enable = true;
    settingsFile = config.sops.secrets.xray-config.path;
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

  services.beszel.agent = {
    enable = true;
    openFirewall = true;
    environment = {
      KEY = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMP1Jz0wDMLzJSsQeAUCdLq6d92vATrdvBBxnov3pPxj";
    };
  };

  # sops.secrets.explo-env = {};
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

    # explo = {
    #   image = "ghcr.io/lumepart/explo:latest";
    #   environment = {
    #   };
    #   environmentFiles = [
    #     config.sops.secrets.explo-env.path
    #   ];
    # };
  };

  services.immich = {
    enable = true;
  };

  system.stateVersion = "24.05";
}
