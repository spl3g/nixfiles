{ pkgs, ... }: {
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
  ];

  # from nixosModules
  pbutton.disable = true;
  docker.enable = true;
  greetd.command = "Hyprland";

  powerManagement.enable = true;
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 20;

      USB_AUTOSUSPEND = 0;
    };
  };

  time.timeZone = "Europe/Moscow";
  networking.hostName = "ltrr-mini";
  networking.hosts = {
    "127.0.0.1" = [ "mr.local" "local.oneln.ru" ];
    "127.0.0.3" = [ "local-api.oneln.ru" ];
  };

  programs.adb.enable = true;
  services.udev.packages = [
    pkgs.android-udev-rules
  ];

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.05";
}
