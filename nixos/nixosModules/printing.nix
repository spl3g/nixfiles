{ pkgs, config, lib, ... }:

{
  options = {
    printing.enable = lib.mkEnableOption "enable printing";
  };
  config = lib.mkIf config.printing.enable {
    services = {
      printing.enable = true;
      printing.drivers = [ pkgs.hplipWithPlugin ];
      avahi = {
        enable = true;
        nssmdns = true;
        openFirewall = true;
      };
    };
  };
}
