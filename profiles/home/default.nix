{
  inputs,
  self,
  pkgs,
  config,
  lib,
  modulesPath,
  globals,
  ...
}:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")

    self.nixosModules.system.directories
    self.nixosModules.system.nix
    self.nixosModules.services.kube

    inputs.disko.nixosModules.disko
  ];

  networking.firewall = {
    trustedInterfaces = ["wg0"];
    allowedTCPPorts = [6443];
  };

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = true;
  };

  environment.systemPackages = with pkgs; [
    curl
    gitMinimal
  ];
  
  users.users = {
    root = {
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDJ8UW1BXDGDmlaiARO3a9boTG8wknUyITMz0Z0OJpHx spleefer6@yandex.ru"
      ];
    };
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

  createPaths."/srv/files" = {
    owner = "files";
    permissions = "0770";
    group = "files";
    subPaths = {
      "music".group = "music";
      "images".group = "images";
    };
  };

  age.secrets.wg-priv-key = {
    rekeyFile = ../../secrets/home/wg-priv.key.age;
  };

  networking.wg-quick = {
    interfaces.wg0 = {
      address = ["10.1.1.4/32"];
      listenPort = 51820;

      privateKeyFile = config.age.secrets.wg-priv-key.path;

      peers = [
        {
          endpoint = "${globals.domain}:51820";
          publicKey = "1RwEOL8br97Mujhz3fkfYKcxUFNHYAmt5JbWTbR3ihE=";
          allowedIPs = ["10.1.1.1/32"];
          persistentKeepalive = 25;
        }
      ];
    };
  };

  age.secrets.kube-sops-key = {
    rekeyFile = ../../secrets/home/kube-sops.key.age;
  };

  age.secrets.kube-token = {
    rekeyFile = ../../secrets/home/kube-token.key.age;
  };

  kube = {
    enable = true;
    tokenFile = config.age.secrets.kube-token.path;
    sopsKeyFile = config.age.secrets.kube-sops-key.path;
    nodeIP = "10.1.1.4";
  };
}
