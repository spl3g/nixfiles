{ pkgs, ... }:
let
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
  iconTheme = {
  };
in
{
  home.packages = [ pkgs.libnotify ];
  services.dunst = {
    enable = false;
    inherit settings;
  };
}
