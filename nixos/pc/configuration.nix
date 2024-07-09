{ inputs, outputs, lib, config, pkgs, ... }: {
  imports = [
    ../general.nix
    ./hardware-configuration.nix
    ../nixosModules/printing.nix
 ];

  time.timeZone = "Asia/Yekaterinburg";
  networking.hostName = "ltrr";

  printing.enable = true;

  services.xserver = {
    enable = true;
    displayManager.startx.enable = true;
  };

  services.openssh.enable = true;
}

  
