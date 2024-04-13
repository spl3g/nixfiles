{ pkgs, ... }:

{
  home.packages = with pkgs; with python311Packages; [
    # required dependencies
    ripgrep
    fd
    tree-sitter
    emacs-all-the-icons-fonts
    libappindicator
    poppler_utils
    emacs-lsp-booster
    nixd
    sqlite
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
  };
  xdg.configFile = {
    # "emacs/init.el".text = builtins.readFile ./init.el;
    "emacs/early-init.el".text = builtins.readFile ./early-init.el;
  };
}
