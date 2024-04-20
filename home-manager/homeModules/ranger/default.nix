{ pkgs, config, lib, ... }:

{
  options = {
    ranger.enable = lib.mkEnableOption "enable ranger";
  };
  config = lib.mkIf config.ranger.enable {
    home.packages = with pkgs; [ ranger ];
    xdg.configFile = {
      "ranger/rc.conf".text = builtins.readFile ./rc.conf;
      "ranger/rifle.conf".text = builtins.readFile ./rifle.conf;
      "ranger/scope.sh".text = builtins.readFile ./scope.sh;
      "ranger/commands.py".source = ./commands.py; 
      "ranger/plugins".source = ./plugins;
    };
  };
}
