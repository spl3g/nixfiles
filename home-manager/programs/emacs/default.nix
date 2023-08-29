{ pkgs, ... }:
let
  pkgsForEmacs = with pkgs; [
    tree-sitter
    gcc
    cmake
    gnumake
  ];
  pkgsUsePackage = with pkgs; [
    (pkgs.emacsWithPackagesFromUsePackage {
      inherit (service) package;
      config = ./init.el;
      alwaysEnsure = true;
      extraEmacsPackages = epkgs: [
        epkgs.use-package
      ];
    })
  ];
in
{
  home.packages = pkgsForEmacs ++ pkgsUsePackage;
  xdg.configFile = {
    "emacs/init.el".text = builtins.readFile ./init.el;
    "emacs/early-init.el".text = builtins.readFile ./early-init.el;
  };
}
