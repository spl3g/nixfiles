{
  lib,
  modulesPath,
  ...
}:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")

    ./disk-config.nix
  ];
  
  networking.hostName = "ltrr-cloud";
  networking = {
    interfaces.ens3 = {
      ipv4.addresses = [
        {
          address = "82.108.206.49";
          prefixLength = 32;
        }
      ];
    };
    defaultGateway = {
      address = "82.108.206.1";
      interface = "ens3";
    };

    nameservers = ["8.8.8.8" "1.1.1.1"];
    useDHCP = lib.mkDefault false;
  };

  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  age.rekey = {
    hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMzY0BcI9AxK01khMSUH8/49MnBc7th0eJ0pnC64IK21";
    masterIdentities = ["/home/jerpo/.config/age/keys.txt"];
    storageMode = "local";
    localStorageDir = ./. + "/secrets/rekeyed";
  };

  system.stateVersion = "24.05";
}
