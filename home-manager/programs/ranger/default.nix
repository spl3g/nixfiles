{ pkgs, ... }:
{
  home.packages = with pkgs; [ ranger wl-clipboard ];
  xdg.configFile = {
    "ranger/rc.conf".text = builtins.readFile ./rc.conf;
    "ranger/rifle.conf".text = builtins.readFile ./rifle.conf;
    "ranger/scope.sh".text = builtins.readFile ./scope.sh;
    "ranger/commands.py".source = ./commands.py; 
    "ranger/plugins".source = ./plugins;
  };
}
