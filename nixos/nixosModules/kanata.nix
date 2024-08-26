{ config, lib, ... }:

{
  options = {
     kanata.enable = lib.mkEnableOption "enable kanata";
  };

  config = lib.mkIf config.kanata.enable {
    services.kanata = {
      enable = true;
      keyboards = {
        board = {
          config = ''
            (defsrc
              caps)
            (defalias
              caps (tap-hold 100 150 esc lctl))
            (deflayer base
              @caps)
            '';
        };
      };
    };
  };
}
