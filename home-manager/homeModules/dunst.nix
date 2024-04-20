{ pkgs, config, lib, ... }:

{
  options = {
    dunst.enable = lib.mkEnableOption "enable dunst";
  };
  config = lib.mkIf config.dunst.enable {
    home.packages = [ pkgs.libnotify ];
    services.dunst = {
      enable = false;
      settings = {
        global = {
          origin = "bottom-right";
          frame_color = "#c4a7e7";
          notification_limit = 5;
          progress_bar = true;
          background = "#232136";
          foreground = "#e0def4";
          separator_color = "foreground";
        };
      };
    };
  };
}
