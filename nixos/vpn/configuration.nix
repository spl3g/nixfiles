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
  ];

  networking.domain = "kcu.su";
  networking.hostName = "ltrr-vpn";
  networking = {
    interfaces.ens3 = {
      ipv4.addresses = [{
        address = "64.188.126.186";
        prefixLength = 32;
      }];
    };
    defaultGateway = {
      address = "100.64.0.1";
      interface = "ens3";
    };
  };

  networking.useDHCP = lib.mkDefault false;

  networking.nameservers = [ "8.8.8.8" "1.1.1.1" ];

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
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPuQVHIGm2bfvhW16ZI/4hDK2X8W+ADbPLXwzKZIYXZL user@LAPTOP-72FMD6D0"
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

  system.stateVersion = "24.05";
}
