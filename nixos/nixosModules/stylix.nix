{ pkgs, config, lib, outputs, ... }:

{
  options = {
    stylix.enable = lib.mkEnableOption "enable stylix";
  };
  config = lib.mkIf config.stylix.enable {
    stylix = with outputs.homeConfigurations."jerpo@ltrr-mini".config.stylix; {
      image = image;
      base16Scheme = base16Scheme;
      autoEnable = false;
      targets.console.enable = true;
      fonts = with fonts; {
        serif = serif;

        sansSerif = sansSerif;

        monospace = monospace;

        emoji = emoji;
      };
    };
  };
}
