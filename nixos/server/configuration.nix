{ modulesPath, config, lib, pkgs, ... }: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ./disk-config.nix
  ];
  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

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
    fimoz = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ//SNTK/qZmoT0YH7tHxXg6VGEbxbxPLXQVC1zsBPb4 90ts@mail.ru"
      ];
    };
  };

  networking.firewall.allowedTCPPorts = [ 80 443 57625 ];

  
  security.acme = {
    acceptTerms = true;
    defaults.email = "notspl3g+acme@duck.com";
  };

  services.nginx = {
    enable = true;
    virtualHosts = {
      "xray.kcu.su" = {
        forceSSL = true;
        enableACME = true;

        locations."/" = {
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
        };
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
