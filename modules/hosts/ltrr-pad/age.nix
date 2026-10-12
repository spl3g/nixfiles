{inputs, ...}: {
  flake.nixosModules.ltrr-pad = {
    age.rekey = {
      hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMdLeTuUxVIdgYCW74OUpXGQI0vTgYYc3dIIBwtX1/4h";
      masterIdentities = ["/home/jerpo/.config/age/keys.txt"];
      storageMode = "local";
      localStorageDir = ./. + "/secrets/rekeyed";
    };
  };
}
