{ pkgs, config, lib, inputs, outputs, ... }:

{
  imports = [
    inputs.stylix.nixosModules.stylix
  ];
  
  options = {
    greetd.enable = lib.mkEnableOption "enable greetd";
  };
  config = lib.mkIf config.greetd.enable {
    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.greetd.tuigreet}/bin/tuigreet -t -r -c Hyprland";
          user = "jerpo";
        };
      };
    };
  };
}
