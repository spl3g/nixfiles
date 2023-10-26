{ pkgs, ... }:
let
  pkgsForEmacs = with pkgs; [
    tree-sitter
    gcc
    cmake
    nodePackages_latest.bash-language-server
    gnumake
    tdlib
    emacs-all-the-icons-fonts
    libappindicator
    libwebp
    tgs2png
    pkg-config
    ffmpeg_6-full
    rustc
    cargo
    rust-analyzer
    nodejs_20
  ];
  pythonPkgs = with pkgs.python311Packages; [
    flake8
    python-lsp-server
    autopep8
    matplotlib
  ];
  
  pkgsUsePackage = with pkgs; [
    (pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs29;
      config = ./init.el;
      alwaysEnsure = true;
      extraEmacsPackages = epkgs: [
        epkgs.use-package
        (epkgs.melpaBuild rec {
          pname = "codeium";
          version = "1.2.102";

          src = fetchFromGitHub {
            owner = "Exafunction";
            repo = "codeium.el";
            rev = "1.2.102";
            sha256 = "0slc13d9nxkn12fw640n1l721qvhnjp7yy3yc7av4c58nl9yv40z";
          };

          commit = "915837df0f41397028f4ad34f43722c61efd298d";

          recipe = writeText "recipe" ''
          (codeium
            :repo "${src.owner}/${src.repo}"
            :fetcher github)
          '';
        })
      ];
    })
  ];
in
{
  home.packages = pkgsForEmacs ++ pkgsUsePackage ++ pythonPkgs;
  xdg.configFile = {
    # "emacs/init.el".text = builtins.readFile ./init.el;
    "emacs/early-init.el".text = builtins.readFile ./early-init.el;
  };
}
