{inputs, ...}: {
  flake.nixosModules.ltrr-block = {
    age.rekey = {
      hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINe2bfzslwh9DwNXopmaiRKVNQMIQNuMlP/jJCDrwSbc";
      masterIdentities = ["/home/jerpo/.config/age/keys.txt"];
      storageMode = "local";
      localStorageDir = ./. + "/secrets/rekeyed";
    };
  };
}
