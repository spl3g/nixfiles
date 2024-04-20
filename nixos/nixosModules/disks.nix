{ pkgs, config, lib, ... }:

{
  options = {
    disks.enable = lib.mkEnableOption "auto disk mount";
  };
  config = lib.mkIf config.disks.enable {
    services = {
      devmon.enable = true;
      gvfs.enable = true;
      udisks2 = {
        enable = true;
        mountOnMedia = true;
      };
    };
  };
}
