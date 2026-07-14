{
  modulesPath,
  config,
  lib,
  pkgs,
  outputs,
  inputs,
  ...
}: let
  domain = "kcu.su";
in {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    "${inputs.nixpkgs}/nixos/modules/services/networking/headscale.nix"
    ./disk-config.nix
    ../serverModules/nginx.nix
    ../serverModules/directories.nix
  ];

  disabledModules = ["services/networking/headscale.nix"];

  nixpkgs.overlays = [
    outputs.overlays.unstable-packages
  ];

  sops = {
    defaultSopsFile = ../../secrets/ltrr-tw/secrets.yaml;
    defaultSopsFormat = "yaml";
    age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
  };

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

  sops.secrets.wg-private-key = {};
  networking.wg-quick = {
    interfaces.wg0 = {
      address = ["10.1.1.1/32"];
      listenPort = 51820;

      privateKeyFile = config.sops.secrets.wg-private-key.path;

      preUp = ''
        sysctl -w net.ipv4.ip_forward=1
        iptables -t nat -I PREROUTING 1 -i ens3 -p tcp --dport 25565 -j DNAT --to-destination 10.1.1.2:25565
        iptables -A FORWARD -p tcp -d 10.1.1.2 --dport 25565 -j ACCEPT
        iptables -t nat -A POSTROUTING -o wg0 -p tcp --dport 25565 -d 10.1.1.2 -j MASQUERADE
      '';
      postDown = ''
        iptables -t nat -D PREROUTING -i ens3 -p tcp --dport 25565 -j DNAT --to-destination 10.1.1.2:25565
        iptables -D FORWARD -p tcp -d 10.1.1.2 --dport 25565 -j ACCEPT
        iptables -t nat -D POSTROUTING -o wg0 -p tcp --dport 25565 -d 10.1.1.2 -j MASQUERADE
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
  networking.firewall.allowedUDPPorts = [51820];

  security.acme = {
    acceptTerms = true;
    defaults.email = "notspl3g+acme@duck.com";
  };

  nginx = {
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
      homeConfig = import ../server/configuration.nix {inherit modulesPath config lib pkgs outputs inputs;};
    in {
      subdomains = homeConfig.nginx.subdomains;
      url = "http://10.1.1.2";
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
}
