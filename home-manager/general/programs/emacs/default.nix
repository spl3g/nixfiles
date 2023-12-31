{ pkgs, ... }:
let
  pkgsForEmacs = with pkgs; [
    tree-sitter
    emacs-all-the-icons-fonts
    libappindicator
    poppler_utils
    nil
    nodejs
  ];
  
  pkgsUsePackage = with pkgs; [
    (pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs29-pgtk;
      config = ./init.el;
      alwaysEnsure = true;
      # TODO
      # with next flake update delete this, if mood-line hash changes:
      # "commit": "10b5195f1b400d64d646f73a21bf5469612a375b",
      # "sha256": "0wj8rdgsqsdd2ps3w7sj8a7yhhz0iczwgnalalzws42x8s9yn4j0",
      override = final: prev: {
        mood-line = prev.melpaPackages.mood-line.overrideAttrs(old: {
          src = pkgs.fetchgit {
            url = "https://gitlab.com/jessieh/mood-line";
            sha256 = "00vbv40x04g5f9n8i1ylhawslf42fsz0046r3srs4ss6pq8s893r";
            rev = "10b5195f1b400d64d646f73a21bf5469612a375b";
          };
        });
      };
      extraEmacsPackages = epkgs: [
        epkgs.use-package
        # (epkgs.melpaBuild rec {
        #   pname = "codeium";
        #   version = "1.2.102";

        #   src = fetchFromGitHub {
        #     owner = "Exafunction";
        #     repo = "codeium.el";
        #     rev = "1.4.4";
        #     sha256 = "1jjix7fn73ihjnhfivf72wris72f4kwf7xb6k5hxs41fm4kr9hdd";
        #   };

        #   commit = "ddc9927ea231ecc5a32f7c9905f92fdfb7912e75";

        #   recipe = writeText "recipe" ''
        #   (codeium
        #     :repo "${src.owner}/${src.repo}"
        #     :fetcher github)
        #   '';
        # })
        # (epkgs.melpaBuild rec {
        #   pname = "telega";
        #   version = "0.8.216";
        #   src = fetchFromGitHub {
        #     owner = "zevlg";
        #     repo = "telega.el";
        #     rev = "3899aa8648b9e6deddbb34a2a817ca18acb9d97a";
        #     sha256 = "05xrm86gp185mgwb62w720hcbn87salk8z0whq6zf2r2f24l6xbw";
        #   };
        #   commit = "3899aa8648b9e6deddbb34a2a817ca18acb9d97a";
        #   recipe = writeText "recipe" ''
        #   (telega :fetcher github
        #     :repo "zevlg/telega.el"
        #     :files (:defaults "etc" "server" "contrib" "Makefile"))
        #   '';
        # })
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
        (epkgs.melpaBuild rec {
          pname = "copilot";
          version = "20231219";

          src = fetchFromGitHub {
            owner = "zerolfx";
            repo = "copilot.el";
            rev = "d4fa14cea818e041b4a536c5052cf6d28c7223d7";
            sha256 = "1bn2im5ybzmwbwbi7v39s0qzmca5isp7zb0ls61y6sramh6k8fsg";
          };
          
          packageRequires = with epkgs; [ editorconfig dash s ];
          
          commit = "d4fa14cea818e041b4a536c5052cf6d28c7223d7";
          
          recipe = writeText "recipe" ''
          (copilot
            :repo "${src.owner}/${src.repo}"
            :fetcher github
            :files ("*.el" "dist"))
          '';
        })
      ];
    })
  ];
in
{
  home.packages = pkgsForEmacs ++ pkgsUsePackage;
  xdg.configFile = {
    # "emacs/init.el".text = builtins.readFile ./init.el;
    "emacs/early-init.el".text = builtins.readFile ./early-init.el;
  };
}
