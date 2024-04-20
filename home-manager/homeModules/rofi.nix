{ pkgs, config, lib, ... }:

{
  options = {
    rofi.enable = lib.mkEnableOption "enable rofi";
    rofi.package = lib.mkPackageOption pkgs "rofi package" { default = [ "rofi-wayland" ]; };
  };
  
  config = lib.mkIf config.rofi.enable {
    programs.rofi = {
      enable = true;
      package = config.rofi.package;
      theme =
        with config.lib.stylix.colors.withHashtag;
        builtins.toFile "theme.rasi" ''
        * {
            font:   "FiraCode Nerd Font Medium 12";
        
            bg0:     ${base01};
            bg1:     ${base02};
            fg0:     ${base04};
        
            accent-color:     ${base03};
            urgent-color:     #ffffff;
        
            background-color:   transparent;
            text-color:         @fg0;
        
            margin:     0;
            padding:    0;
            spacing:    0;
        }

        ${builtins.readFile ./attachments/rofi-theme.rasi}'';
      cycle = true;
      plugins = with pkgs; [
        rofi-emoji
        rofi-calc
      ];
      extraConfig = {
        kb-row-up = "Up,Alt+k";
        kb-row-down = "Down,Alt+j";
      };
    };
  };
}

