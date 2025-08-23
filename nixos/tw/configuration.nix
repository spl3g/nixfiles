{ modulesPath, config, lib, pkgs, ... }: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ./disk-config.nix
    ../serverModules/nginx.nix
  ];

  sops = {
    defaultSopsFile = ../../secrets/ltrr-tw/secrets.yaml;
    defaultSopsFormat = "yaml";
    age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  };
  
  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  networking = {
    interfaces.ens3 = {
      ipv4.addresses = [{
        address = "77.232.139.132";
        prefixLength = 24;
      }];
    };
    defaultGateway = {
      address = "77.232.139.1";
      interface = "ens3";
    };
  };

  networking.nameservers = [ "8.8.8.8" "1.1.1.1" ];

  networking.useDHCP = lib.mkDefault false;

  networking.hostName = "ltrr-tw";

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

  zramSwap = {
    enable = true;
  };

  swapDevices = [{
    device = "/var/lib/swapfile";
    size = 2*1024;
  }];

  networking.nat = {
    enable = true;
    externalInterface = "ens3";
    internalInterfaces = [ "wg0" ];
  };

  sops.secrets.wg_private_key = {};
  networking.wg-quick = {
    interfaces.wg0 = {
      address = [ "10.1.1.1/32" ];
      listenPort = 51820;

      privateKeyFile = config.sops.secrets.wg_private_key.path;

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

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  networking.firewall.allowedUDPPorts = [ 51820 ];
  
  security.acme = {
    acceptTerms = true;
    defaults.email = "notspl3g+acme@duck.com";
  };

  nginx = {
    enable = true;
    acme.enable = true;

    domain = "kcu.su";
    subdomains = {
      "headscale" = {
        proxyPass = "http://127.0.0.1:8768";
        proxyWebsockets = true;
        recommendedProxySettings = true;
      };
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


  services.headscale = {
    enable = true;
    port = 8768;
    settings = {
      server_url = "https://headscale.kcu.su:443";
      dns = {
        base_domain = "tailnet.kcu.su";
        nameservers.global = [ "8.8.8.8" ];
      };
    };
  };

  system.stateVersion = "24.05";
}
