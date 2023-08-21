{ pkgs, ... }:
let
  service = {
    client = {
      enable = true;
      arguments = [ "-c" "-a emacs" ];
    };
    defaultEditor = true;
    startWithUserSession = "graphical";
    package = pkgs.emacs-gtk;
  };
  configs = {
    "emacs/init.el".text = builtins.readFile ./init.el;
    "emacs/early-init.el".text = builtins.readFile ./init.el;
  };
in
{
  services.emacs = {
    enable = true;
    inherit (service);
  };
  programs.emacs = {
    enable = true;
    inherit (service) package;
  };
  xdg.configFile = {
    inherit (configs); 
  };
}
