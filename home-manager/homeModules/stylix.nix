{ pkgs, config, lib, inputs, ... }:

{
  imports = [
    inputs.stylix.homeManagerModules.stylix
  ];

  options = {
    stylixConfig = {
      enable = lib.mkEnableOption "enable stylix";
      theme = lib.mkOption { type = lib.types.str; };
    };
    wallpaper = lib.mkOption { type = with lib.types; oneOf [str path package]; };
  };
  config = lib.mkIf config.stylixConfig.enable {
    wallpaper = with config.lib.stylix.colors.withHashtag;
      pkgs.runCommand "cat.png" {} ''
        pastel=${pkgs.pastel}/bin/pastel
        SHADOWS=$($pastel darken 0.1 '${base05}' | $pastel format hex)
        TAIL=$($pastel lighten 0.1 '${base02}' | $pastel format hex)
        HIGHLIGHTS=$($pastel lighten 0.1 '${base05}' | $pastel format hex)

        ${pkgs.imagemagick}/bin/convert ${./attachments/basecat.png} \
	        -fill '${base00}' -opaque black \
	        -fill '${base05}' -opaque white \
	        -fill '${base08}' -opaque blue \
	        -fill $SHADOWS -opaque gray \
	        -fill '${base02}' -opaque orange \
	        -fill $TAIL -opaque green \
	        -fill $HIGHLIGHTS -opaque brown \
	        $out'';
    stylix = {
      enable = true;
      targets = {
        rofi.enable = false;
        waybar.enable = false;
      };

      polarity = "dark";

      opacity = {
        terminal = 0.7;
        # applications = 0.7;
      };
      
      base16Scheme = "${pkgs.base16-schemes}/share/themes/${config.stylixConfig.theme}.yaml";
      image = config.wallpaper;
      cursor = {
        package = pkgs.bibata-cursors;
        name = "Bibata-Modern-Ice";
        size = 24;
      };
      
      fonts = {
        serif = {
          package = pkgs.noto-fonts;
          name = "Noto Serif";
        };

        sansSerif = {
          package = pkgs.rubik;
          name = "Rubik";
        };

        monospace = {
          package = pkgs.nerd-fonts.fira-code;
          name = "FiraCode Nerd Font";
        };

        emoji = {
          package = pkgs.noto-fonts-emoji;
          name = "Noto Color Emoji";
        };
      };
    };
  };
}
