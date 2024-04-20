{ pkgs, config, lib, ... }:

{
  options = {
    mako.enable = lib.mkEnableOption "enable mako";
  };
  
  config = lib.mkIf config.mako.enable {
    services.mako = {
      enable = true;
      anchor = "bottom-right";
      # backgroundColor = "#1f1d2e";
      # borderColor = "#e0def4";
      # textColor = "#e0def4";
      defaultTimeout = 5000;
      borderSize = 3;
      borderRadius = 7;
      # font = "Rubik 11";
    };
  };
}
