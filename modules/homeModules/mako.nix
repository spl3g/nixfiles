{inputs, ...}: {
  flake.homeModules.mako = {
    pkgs,
    config,
    lib,
    ...
  }: {
    options.customs = {
      mako.enable = lib.mkEnableOption "enable mako";
    };

    config = lib.mkIf config.customs.mako.enable {
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
  };
}
