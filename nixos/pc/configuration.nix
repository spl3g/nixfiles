{ pkgs, ... }: {
  imports = [
    ../general.nix
    ./hardware-configuration.nix
    ../nixosModules/printing.nix
    ../nixosModules/greetd.nix
 ];

  time.timeZone = "Asia/Yekaterinburg";
  networking.hostName = "ltrr";

  printing.enable = true;

  programs.adb.enable = true;
  services.udev.packages = [
    pkgs.android-udev-rules
  ];

  services.xserver = {
    enable = true;
    displayManager.startx.enable = true;
  };

  services.openssh.enable = true;
  security.polkit = {
    enable = true;
  };
  environment.systemPackages = with pkgs; [
    lxqt.lxqt-policykit
  ];

  greetd.command = "startx";

  system.stateVersion = "24.11";
}

  
