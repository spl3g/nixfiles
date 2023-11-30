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
      package = pkgs.emacs29-pgtk;
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
            rev = "1.4.4";
            sha256 = "1jjix7fn73ihjnhfivf72wris72f4kwf7xb6k5hxs41fm4kr9hdd";
          };

          commit = "ddc9927ea231ecc5a32f7c9905f92fdfb7912e75";

          recipe = writeText "recipe" ''
          (codeium
            :repo "${src.owner}/${src.repo}"
            :fetcher github)
          '';
        })
        (epkgs.melpaBuild rec {
          pname = "telega";
          version = "0.8.216";
          src = fetchFromGitHub {
            owner = "zevlg";
            repo = "telega.el";
            rev = "3899aa8648b9e6deddbb34a2a817ca18acb9d97a";
            sha256 = "05xrm86gp185mgwb62w720hcbn87salk8z0whq6zf2r2f24l6xbw";
          };
          commit = "3899aa8648b9e6deddbb34a2a817ca18acb9d97a";
          recipe = writeText "recipe" ''
          (telega :fetcher github
            :repo "zevlg/telega.el"
            :files (:defaults "etc" "server" "contrib" "Makefile"))
          '';
        })
        # (epkgs.melpaBuild rec {
        #   pname = "orgnote";
        #   version = "0.7.17";

        #   src = fetchFromGitHub {
        #     owner = "Artawower";
        #     repo = "orgnote.el";
        #     rev = "v${version}";
        #     sha256 = "1lrj47h244z4dqq2wyhpww7p3b4sy6bayk8lwlka517lhbcdgh33";
        #   };

        #   commit = "ccc40cc346ebf5a6e6a55e3d4a147f0230337350";
          
        #   recipe = writeText "recipe" ''
        #   (orgnote
        #     :repo "${src.owner}/${src.repo}"
        #     :fetcher github)
        #   '';
        # })
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
