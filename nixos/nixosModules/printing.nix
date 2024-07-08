{ pkgs, config, lib, ... }:

{
  options = {
    printing.enable = lib.mkEnableOption "enable printing";
  };
  config = lib.mkIf config.printing.enable {
    hardware.sane = {
      enable = true;
      extraBackends = [ pkgs.hplipWithPlugin ];
    };
    services = {
      printing.enable = true;
      printing.drivers = [ pkgs.hplipWithPlugin ];
      avahi = {
        enable = true;
        nssmdns4 = true;
        openFirewall = true;
      };
    };
  };
}
