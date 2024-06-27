{ pkgs, config, lib, inputs, outputs, ... }:

{
  imports = [
    inputs.stylix.nixosModules.stylix
  ];
  
  options = {
    stylixConfig.enable = lib.mkEnableOption "enable stylix";
  };
  config = lib.mkIf config.stylixConfig.enable {
    stylix = with outputs.homeConfigurations."jerpo@ltrr-mini".config.stylix; {
      enable = true;
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
