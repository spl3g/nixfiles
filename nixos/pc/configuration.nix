{ inputs, outputs, lib, config, pkgs, ... }: {
  imports = [
    ../general.nix
    inputs.home-manager.nixosModules.home-manager
  ];

  time.timeZone = "Europe/Yekaterinburg";
  networking.hostName = "ltrr";

  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;

    # wm
    windowManager.bspwm.enable = true;
    
    # Layout
    layout = "us,ru";
    xkbOptions = "grp:win_space_toggle";
  };

  # Printing
  services = {
    printing.enable = true;
    printing.drivers = [ pkgs.hplipWithPlugin ];
    avahi = {
      enable = true;
      nssmdns = true;
      openFirewall = true;
    };
  };
  
  home-manager = {
    extraSpecialArgs = { inherit inputs outputs; };
    useGlobalPkgs = true;
    useUserPackages = true;
    users.jerpo = import ../home-manager/pc;
  };

}
