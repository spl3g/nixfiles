{inputs, ...}: {
  flake.nixosModules.ltrr-cloud = {
    age.rekey = {
      hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMzY0BcI9AxK01khMSUH8/49MnBc7th0eJ0pnC64IK21";
      masterIdentities = ["/home/jerpo/.config/age/keys.txt"];
      storageMode = "local";
      localStorageDir = ./. + "/secrets/rekeyed";
    };
  };
}
