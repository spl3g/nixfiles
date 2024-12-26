{ config, lib, ... }:

{
  options = {
    ly.enable = lib.mkEnableOption "enable ly desktop manager";
  };

  config = lib.mkIf config.ly.enable {
    services.displayManager.ly = {
      enable = true;
      settings = {
        save = true;
      };
    };
  };
}
