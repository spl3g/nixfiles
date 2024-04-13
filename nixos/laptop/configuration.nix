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
    inputs.home-manager.nixosModules.home-manager
  ];

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet -t -r -c Hyprland";
        user = "jerpo";
      };
    };
  };
                     
  programs.niri.enable = true;

  time.timeZone = "Europe/Moscow";
  networking.hostName = "ltrr-mini";

  virtualisation.libvirtd.enable = true;
  programs.virt-manager.enable = true;
  
  services.xserver = {
    enable = false;
    displayManager.lightdm.enable = false;
  };

  environment.systemPackages = with pkgs; [
    joycond-cemuhook
  ];

  virtualisation.docker = {
    enable = true;
    liveRestore = true;
    daemon.settings = {
      bip = "172.20.0.1/16";
      default-address-pools = [{
        base = "172.20.0.0/8";
        size = 16;
      }];
    };
  };

  # virtualisation.waydroid.enable = true;
  
  programs.adb.enable = true;
  services.udev.packages = [
    pkgs.android-udev-rules
  ];

  services.joycond.enable = true;

  home-manager = {
    extraSpecialArgs = { inherit inputs outputs; };
    useGlobalPkgs = true;
    useUserPackages = true;
    users.jerpo = import ../../home-manager/laptop;
  };

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
    HandleLidSwitch=suspend
    HandleLidSwitchExternalPower=suspend
  '';


  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.05";
}
