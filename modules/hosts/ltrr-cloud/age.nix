{inputs, ...}: {
  flake.nixosModules.ltrr-cloud = {
    age.rekey = {
      hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGkmaIw2J4H6lWDVnopnUKQuQMJuQf5VMoC1/YwCuhAb";
      masterIdentities = ["/home/jerpo/.config/age/keys.txt"];
      storageMode = "local";
      localStorageDir = ./. + "/secrets/rekeyed";
    };
  };
}
