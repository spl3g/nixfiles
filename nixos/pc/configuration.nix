{ inputs, outputs, pkgs, ... }: {
  imports = [
    ../general.nix
    inputs.home-manager.nixosModules.home-manager
  ];

  time.timeZone = "Asia/Yekaterinburg";
  networking.hostName = "ltrr";

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
}
