{
  self,
  config,
  inputs,
  ...
}:
{
  imports = [
    ./disk-config.nix
    ./hardware-configuration.nix
  ];
  
  networking.hostName = "ltrr-pad";  
  networking.nftables.enable = true;
  networking.networkmanager.enable = true;

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  age.rekey = {
    hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMdLeTuUxVIdgYCW74OUpXGQI0vTgYYc3dIIBwtX1/4h";
    masterIdentities = ["/home/jerpo/.config/age/keys.txt"];
    storageMode = "local";
    localStorageDir = ./. + "/secrets/rekeyed";
  };

  kube.nodeLabel = [config.networking.hostName];

  system.stateVersion = "24.05";
}
