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
              caps lctrl)
            (deflayer base
              @caps)
            '';
        };
      };
    };
  };
}
