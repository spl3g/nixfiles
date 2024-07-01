{ pkgs, config, lib, inputs, ... }:

{
  options = {
    firefox.enable = lib.mkEnableOption "enable firefox";
  };
  config = lib.mkIf config.firefox.enable {
    programs.firefox = {
      enable = true;
      profiles.ShyFox = {
        isDefault = true;
        extensions = with config.nur.repos.rycee.firefox-addons; [
          bitwarden
          ublock-origin
          sponsorblock
          return-youtube-dislikes
          firefox-color
          tampermonkey
          duckduckgo-privacy-essentials
          sidebery
          mal-sync
        ];

        extraConfig = builtins.readFile "${inputs.shyfox.outPath}/user.js";
        search = {
          force = true;
          engines = {
            "Brave" = {
              urls = [{ template = "https://search.brave.com/search?q={searchTerms}"; }];
              iconUpdateURL = "https://cdn.search.brave.com/serp/v2/_app/immutable/assets/safari-pinned-tab.539899c7.svg";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!br" ];
            };
            "NixOS" = {
              urls = [{ template = "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query={searchTerms}"; }];
              iconUpdateURL = "https://nixos.org/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!ns" ];
            };
            "HomeManager" = {
              urls = [{ template = "https://mipmip.github.io/home-manager-option-search/?query={searchTerms}"; }];
              iconUpdateURL = "https://github.com/mipmip/home-manager-option-search/blob/main/images/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!hs" ];
            };
            "NixWiki" = {
              urls = [{ template = "https://nixos.wiki/index.php?search={searchTerms}&go=Go"; }];
              iconUpdateURL = "https://nixos.org/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!nw" ];
            };
            "Kinopoisk" = {
              urls = [{ template = "https://www.kinopoisk.ru/index.php?kp_query={searchTerms}"; }];
              iconUpdateURL = "https://www.kinopoisk.ru/favicon.ico";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!ks" ];
            };
            "AnimeGo" = {
              urls = [{ template = "https://animego.org/search/all?q={searchTerms}"; }];
              iconUpdateURL = "https://animego.org/favicon-32x32.png";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!as" ];
            };
          };
          default = "Brave";
        };
      };
    };
    home.file =
      let
        shyfox = pkgs.runCommand "shyfox-chrome" {} ''
          mkdir $out
          cp -r "${inputs.shyfox}/chrome" $out
          chmod -R 755 $out/chrome
          cp ${config.wallpaper} $out/chrome/wallpaper.png
          sed -i -e 's/#main-window:is(\[sizemode="fullscreen"\], \[titlepreface\*="?"\])/#main-window/g' $out/chrome/ShyFox/shy-controls.css
          sed -z -i -e 's/@import url("ShyFox\/shy-floating-search.css");\n//g' $out/chrome/userChrome.css
          sed -i -e 's/content: "Open Sidebery!";/content: none;/g' $out/chrome/ShyFox/shy-sidebar.css
        '';
      in {
      "chrome" = {
        source = "${shyfox}/chrome";
        target = ".mozilla/firefox/ShyFox/chrome/";
        recursive = true;
      };
    };
  };
}
