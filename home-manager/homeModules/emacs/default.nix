{
  pkgs,
  config,
  lib,
  ...
}:

let
  mkMutableSymlink = config.lib.meta.mkMutableSymlink;
in
{
  options = {
    emacs.enable = lib.mkEnableOption "enable emacs";
    emacs.package = lib.mkPackageOption pkgs "emacs package" { default = [ "emacs30-pgtk" ]; };
  };
  config = lib.mkIf config.emacs.enable {
    home.sessionVariables.EDITOR = "emacsclient -a emacs";
    home.packages =
      with pkgs;
      with python311Packages;
      [
        # required dependencies
        ripgrep
        fd
        tree-sitter
        emacs-all-the-icons-fonts
        libappindicator
        poppler_utils
        nixd
        alejandra
        sqlite
      ];

    programs.emacs = {
      enable = true;
      package = config.emacs.package;
      extraPackages =
        epkgs: with epkgs; [
          treesit-grammars.with-all-grammars
          mu4e
        ];
    };

    xdg.configFile = {
      "emacs/early-init.el".source = mkMutableSymlink ./early-init.el;
      "emacs/init.el".source = mkMutableSymlink ./init.el;
      "emacs/elpaca.el".source = mkMutableSymlink ./elpaca.el;
      "emacs/etc/tempel/templates.eld".source = mkMutableSymlink ./templates.eld;
      "emacs/etc/eshell/aliases".source = mkMutableSymlink ./aliases;
    };
  };
}
