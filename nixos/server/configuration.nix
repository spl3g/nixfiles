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

  networking.hostName = "ltrr-home";
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

      "music".proxyPass = "http://127.0.0.1:4747";
      "files".proxyPass = "http://127.0.0.1:${toString config.services.filebrowser.settings.port}";
      "track".proxyPass = "http://127.0.0.1:7093";
    };
  };

  
  sops.secrets.wg_private_key = {
    restartUnits = ["wg-quick-wg0.service"];
  };
  networking.wg-quick = {
    interfaces.wg0 = {
      address = [ "10.1.1.2/32" ];
      listenPort = 51820;

      privateKeyFile = config.sops.secrets.wg_private_key.path;

      peers = [
        {
          endpoint = "kcu.su:51820";
          publicKey = "1RwEOL8br97Mujhz3fkfYKcxUFNHYAmt5JbWTbR3ihE=";
          allowedIPs = ["10.1.1.1/32"];
          persistentKeepalive = 25;
        }
      ];
    };
  };


  services.tailscale.enable = true;

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

  gonic = {
    enable = true;
    extraGroups = ["music" "files"];
    musicPaths = ["/srv/files/music"];
    settings = {
      scan-watcher-enabled = true;
    };
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

  services.immich = {
    enable = true;
  };
  
  system.stateVersion = "24.05";
}
