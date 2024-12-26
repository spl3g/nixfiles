{ pkgs, config, lib, ... }:

{
  options = {
    dunst.enable = lib.mkEnableOption "enable dunst";
  };
  config = lib.mkIf config.dunst.enable {
    home.packages = [ pkgs.libnotify ];
    services.dunst = {
      enable = true;
      settings = {
        global = {
          origin = "bottom-right";
          notification_limit = 5;
          progress_bar = true;
        };
      };
    };
  };
}
