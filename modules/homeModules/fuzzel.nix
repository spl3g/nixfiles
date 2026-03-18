{inputs, ...}: {
  flake.homeModules.fuzzel = {
    pkgs,
    config,
    lib,
    ...
  }: {
    options.customs = {
      fuzzel.enable = lib.mkEnableOption "enable fuzzel";
    };

    config = lib.mkIf config.customs.fuzzel.enable {
      programs.fuzzel = {
        enable = true;
        settings = {
          colors = {
            background = "1f1d2eff";
            text = "6e6a86ff";
            selection = "908caaff";
            selection-text = "1f1d2eff";
          };
          main = {
            lines = 9;
            terminal = "alacritty -e";
            vertical-pad = 0;
            horizontal-pad = 0;
          };
          border.width = 0;
        };
      };
    };
  };
}
