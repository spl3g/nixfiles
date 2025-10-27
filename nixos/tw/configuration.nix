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
    "${inputs.nixpkgs}/nixos/modules/services/networking/headscale.nix" # replacing the options with ones for a newer version
    ./disk-config.nix
    ../serverModules/nginx.nix
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
          address = "77.232.139.132";
          prefixLength = 24;
        }
      ];
    };
    defaultGateway = {
      address = "77.232.139.1";
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

      preUp = "sysctl -w net.ipv4.ip_forward=1";

      peers = [
        {
          allowedIPs = ["10.1.1.2/32"];
          publicKey = "kzXzxJu1AdcRI5UwtGOrN6WuTZYqJo++PYRrXdOu/lY=";
          persistentKeepalive = 25;
        }
      ];
    };
  };

  networking.firewall.allowedTCPPorts = [80 443];
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
    };

    extraVirtualHosts = {
      "_" = {
        locations."/" = {
          return = 444;
        };
      };
    };

    home = let
      homeConfig = import ../server/configuration.nix {inherit modulesPath config lib pkgs;};
    in {
      subdomains = homeConfig.nginx.subdomains;
      url = "http://10.1.1.2";
    };
  };

  services.headscale = {
    enable = true;
    package = pkgs.unstable.headscale;
    port = 8768;
    settings = {
      server_url = "https://headscale.${domain}:443";
      dns = {
        base_domain = "tailnet.${domain}";
        nameservers.global = ["8.8.8.8"];
      };
    };
  };

  system.stateVersion = "24.05";
}
