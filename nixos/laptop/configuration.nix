{ inputs, outputs, pkgs, ... }: {
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
  ];

  # from nixosModules
  pbutton.disable = true;
  docker.enable = true;

  time.timeZone = "Europe/Moscow";
  networking.hostName = "ltrr-mini";

  virtualisation.libvirtd.enable = true;
  programs.virt-manager.enable = true;
  
  programs.adb.enable = true;
  services.udev.packages = [
    pkgs.android-udev-rules
  ];
  
  services.joycond.enable = true;
  environment.systemPackages = with pkgs; [
    joycond-cemuhook
  ];

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.05";
}
