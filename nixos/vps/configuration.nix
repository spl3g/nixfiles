{ modulesPath, config, lib, pkgs, ... }: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ./disk-config.nix
    ../serverModules/nginx.nix
    ../serverModules/files.nix
    ../serverModules/gonic.nix
  ];

  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  networking.firewall.allowedTCPPorts = [
    # http
    80
    443 
    # xray
    57625 
    # wireguard
    53
    5201
  ];
  
  networking.firewall.allowedUDPPorts = [
    # wireguard
    53 
    51820
  ];

  networking.domain = "kcu.su";
  networking.hostName = "ltrr-cloud";
  networking = {
    interfaces.ens3 = {
      ipv4.addresses = [{
        address = "147.45.40.6";
        prefixLength = 32;
      }];
    };
    defaultGateway = {
      address = "10.0.0.1";
      interface = "ens3";
    };
  };

  networking.useDHCP = lib.mkDefault false;

  networking.nameservers = [ "8.8.8.8" "1.1.1.1" ];
  networking.hosts = {
    "192.168.100.100" = ["stereotyped-sheet.aeza.network"];
    "147.45.40.6" = ["stereotyped-sheet.aeza.network" "stereotyped-sheet"];
  };

  networking.nat = {
    enable = true;
    externalInterface = "ens3";
    internalInterfaces = [ "wg0" ];
  };
  
  networking.wg-quick = {
    interfaces.wg0 = {
      address = [ "10.1.1.1/32" ];
      listenPort = 51820;

      privateKeyFile = "/root/wireguard-keys/private";

      preUp = "sysctl -w net.ipv4.ip_forward=1";

      peers = [
        {
          allowedIPs = [ "10.1.1.2/32" ];
          publicKey = "kzXzxJu1AdcRI5UwtGOrN6WuTZYqJo++PYRrXdOu/lY=";
          persistentKeepalive = 25;
        }
      ];
    };
  };

  # services.headscale = {
  #   enable = true;
  #   port = 8123;
  # };
  
  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
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
    fimoz = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ//SNTK/qZmoT0YH7tHxXg6VGEbxbxPLXQVC1zsBPb4 90ts@mail.ru"
      ];
    };
  };


  nginx = {
    enable = true;
    acme.enable = true;
    
    domain = "kcu.su";
    subdomains = {
      "xray" = {
        proxyPass = "http://127.0.0.1:2053";

        extraConfig = "
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header Range $http_range;
          proxy_set_header If-Range $http_if_range; 
          proxy_redirect off;
        ";
        recommendedProxySettings = false;
      };

      "chat".proxyPass = "http://127.0.0.1:30017";
      "argocd".proxyPass = "https://127.0.0.1:30018";
      "health".proxyPass = "http://127.0.0.1:8090";
      "music".proxyPass = "http://127.0.0.1:4747";
    };

    extraVirtualHosts = {
      "_" = {
        locations."/" = {
          return = 444;
        };
      };
    };

    home =
      let
        homeConfig = import ../server/configuration.nix { inherit modulesPath config lib pkgs; };
      in {
        subdomains = homeConfig.nginx.subdomains;
        url = "http://10.1.1.2";
    };
  };

  virtualisation.oci-containers = {
    backend = "docker";
    containers.xui = {
      image = "ghcr.io/mhsanaei/3x-ui:latest";
      ports = ["127.0.0.1:2053:2053" "57625:57625"];
      volumes = [
        "/root/x-ui:/etc/x-ui"
      ];
    };
  };

  services.k3s = {
    enable = false;
    role = "server";
    extraFlags = ["--disable traefik"];
  };

  gonic = {
    enable = true;
    extraGroups = ["music" "files"];
    musicPaths = ["/srv/files/music"];
  };

  boot.supportedFilesystems = [ "nfs" ];  
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
  filesDir = {
    enable = true;
    subPaths = [
      {
        path = "music";
        group = "music";
      }
    ];
  };
  fileSystems."/srv/files/music" = {
    device = "10.1.1.2:/music";
    fsType = "nfs";
    options = [
      "defaults"
      "x-systemd.automount"
      "x-systemd.mount-timeout=30"
      "_netdev"
    ];
  };

  system.stateVersion = "24.05";
}
