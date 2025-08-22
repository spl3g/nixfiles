{
  modulesPath,
  lib,
  pkgs,
  config,
  ...
}:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ./disk-config.nix
    ../serverModules/nginx.nix
    ../serverModules/files.nix
    ../serverModules/nfs.nix
  ];

  nixpkgs.config.allowUnfree = true;

  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  services.openssh.enable = true;

  environment.systemPackages = map lib.lowPrio [
    pkgs.curl
    pkgs.gitMinimal
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

  networking.hostName = "ltrr-server";
  networking.firewall = {
    allowedTCPPorts = [ 80 5030 2049 ];
    allowedUDPPorts = [ 51820 ];
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "notspl3g+acme@duck.com";
  };

  nginx = {
    enable = true;
    domain = "kcu.su";

    recommendedProxySettings = false;
    subdomains = {
      # "cloud" = {
      #   proxyPass = "http://127.0.0.1:9200";
      #   extraConfig = ''
      #     proxy_set_header Host $host;
      #   '';
      # };
      "slskd" = {
        proxyPass = "http://127.0.0.1:5030";
        proxyWebsockets = true;
      };
      
      "files".proxyPass = "http://127.0.0.1:9337";
      "track".proxyPass = "http://127.0.0.1:7093";
    };
  };

  networking.wg-quick = {
    interfaces.wg0 = {
      address = [ "10.1.1.2/32" ];
      listenPort = 51820;

      privateKeyFile = "/root/wireguard-keys/private";

      peers = [
        {
          endpoint = "147.45.40.6:51820";
          publicKey = "12UX8icwCjIfADoX1zhv6QvKrSjMcuoSsKbn51Mr/D8=";
          allowedIPs = ["10.1.1.1/32"];
          persistentKeepalive = 25;
        }
      ];
    };
  };

  # services.opencloud = {
  #   enable = true;
  #   group = "files";
  #   url = "https://cloud.kcu.su";
  #   environment = {
  #     OC_INSECURE = "true";
  #     PROXY_TLS = "false";
  #   };
  # };

  users.users.filebrowser.extraGroups = [ "music" "images" ];
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
  users.users.slskd.extraGroups = [ "files" ];
  services.slskd = {
    enable = true;
    environmentFile = "/var/lib/slskd/env";
    group = "music";
    settings = {
      shares.directories = [ "/srv/files/music" ];
      directories.downloads = "/srv/files/slskd";
    };
    openFirewall = true;
    domain = null;
  };

  virtualisation.oci-containers.backend = "docker";
  virtualisation.oci-containers.containers.traggo = {
    image = "traggo/server";
    ports = [
      "127.0.0.1:7093:3030"
    ];
    environmentFiles = [ "/var/lib/traggo/env" ];
    workdir = "/opt/traggo/";
    volumes = [
      "/opt/traggo/data:/opt/traggo/data"
    ];
  };

  nfs.server = {
    enable = true;
    defaultExportIps = ["10.1.1.0/24"];

    exportDirs = [
      {path = "/srv/files/music";}
    ];
  };
  services.nfs.idmapd = {
    settings = {
      General = {
        Domain = "kcu.su";
      };
      Mapping = {
        Nobody-User = "nobody";
        Nobody-Group = "nogroup";
      };
    };
  };
  
  system.stateVersion = "24.05";
}

