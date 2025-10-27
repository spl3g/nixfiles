{pkgs, ...}: {
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/nixos):
    # outputs.nixosModules.example

    # Or modules from other flakes (such as nixos-hardware):
    # inputs.hardware.nixosModules.common-cpu-amd
    # inputs.hardware.nixosModules.common-ssd

    ../general.nix
    ./hardware-configuration.nix
    ../nixosModules/powerbutton.nix
    ../nixosModules/docker.nix
    ../nixosModules/greetd.nix
    ./disk-config.nix
  ];

  # from nixosModules
  pbutton.disable = true;
  greetd.command = "Hyprland";

  programs.gamescope = {
    enable = true;
    capSysNice = false;
  };

  time.timeZone = "Europe/Moscow";
  networking.hostName = "ltrr-mini";
  networking.hosts = {
    "127.0.0.1" = ["mr.local" "local.oneln.ru"];
    "127.0.0.3" = ["local-api.oneln.ru"];
  };

  programs.adb.enable = true;
  services.udev.packages = [
    pkgs.android-udev-rules
  ];

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.05";
}
