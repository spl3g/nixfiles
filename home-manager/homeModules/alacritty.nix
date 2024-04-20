{ pkgs, config, lib, ... }:

{
  options = {
    alacritty.enable = lib.mkEnableOption "enable alacritty";
  };
  
  config = lib.mkIf config.alacritty.enable {
    programs.alacritty = {
      enable = true;
      settings = {
        cursor = {
          style = "Beam";
          thickness = 0.25;
        };
        window = {
          padding = {
            x = 10;
          };
        };
      };
    };
  };
}


  
