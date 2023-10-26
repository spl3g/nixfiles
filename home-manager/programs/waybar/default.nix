{ pkgs, ... }:
let
  systemd.enable = true;
  package = pkgs.waybar.override {
    swaySupport = false;
    mpdSupport = false;
  };
 
in
{
  programs.waybar = {
    enable = true;
    inherit systemd package;
  };
  xdg.configFile = {
    "waybar/config".text = builtins.readFile ./config.jsonc;
    "waybar/style.css".text = builtins.readFile ./style.css;
  };
}
