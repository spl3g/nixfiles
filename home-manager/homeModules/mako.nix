{
  pkgs,
  config,
  lib,
  ...
}: {
  options = {
    mako.enable = lib.mkEnableOption "enable mako";
  };

  config = lib.mkIf config.mako.enable {
    services.mako = {
      enable = true;
      settings = {
        anchor = "bottom-right";
        default-timeout = 5000;
        border-size = 3;
        border-radius = 7;
      };
    };
  };
}
