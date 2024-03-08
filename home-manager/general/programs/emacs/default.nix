{ pkgs, ... }:

{
  services.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
  };
  home.packages = with pkgs; with python311Packages; [
    tree-sitter
    emacs-all-the-icons-fonts
    libappindicator
    poppler_utils
    emacs-lsp-booster
    nixd
  ];
  xdg.configFile = {
    # "emacs/init.el".text = builtins.readFile ./init.el;
    "emacs/early-init.el".text = builtins.readFile ./early-init.el;
  };
}
