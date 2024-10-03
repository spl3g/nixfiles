{ pkgs, config, lib, ... }:

let
  mkMutableSymlink = config.lib.meta.mkMutableSymlink;
in
{
  options = {
    emacs.enable = lib.mkEnableOption "enable emacs";
  };
  config = lib.mkIf config.emacs.enable {
    home.packages = with pkgs; with python311Packages; [
      # required dependencies
      ripgrep
      fd
      tree-sitter
      emacs-all-the-icons-fonts
      libappindicator
      poppler_utils
      nil
      sqlite
    ];
    
    programs.emacs = {
      enable = true;
      package = pkgs.emacs30-pgtk;
      extraPackages = epkgs: with epkgs; [
        treesit-grammars.with-all-grammars
        mu4e
      ];
    };
    
    xdg.configFile = {
      "emacs/config.org".source = mkMutableSymlink ./config.org;
      "emacs/early-init.el".source = mkMutableSymlink ./early-init.el;
      "emacs/init.el".source = mkMutableSymlink ./init.el;
      "emacs/etc/tempel/templates.eld".source = mkMutableSymlink ./templates.eld;
      "emacs/etc/eshell/aliases".source = mkMutableSymlink ./aliases;
    };
  };
}
