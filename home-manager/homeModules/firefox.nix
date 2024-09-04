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

        extraConfig =
          let
            shyfox = builtins.readFile "${inputs.shyfox.outPath}/user.js";
            betterfox = builtins.readFile "${inputs.betterfox.outPath}/user.js";
            overrides = ''
              user_pref("shyfox.disable.floating.search", true);
              user_pref("shyfox.remove.window.controls", true);
              user_pref("browser.search.suggest.enabled", true);
              user_pref("browser.urlbar.quicksuggest.enabled", true);
              user_pref("browser.urlbar.suggest.quicksuggest.nonsponsored", true);
              user_pref("captivedetect.canonicalURL", "http://detectportal.firefox.com/canonical.html");
              user_pref("network.captive-portal-service.enabled", true);
              user_pref("network.connectivity-service.enabled", true);
            '';
          in
            shyfox + betterfox + overrides;
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
    home.file = {
      "chrome" =
        let
          shyfox = pkgs.runCommand "shyfox-chrome" {} ''
          mkdir $out
          cp -r "${inputs.shyfox}/chrome" $out
          chmod -R 755 $out/chrome
          cp ${config.wallpaper} $out/chrome/wallpaper.png
          echo -e "browser {\n  margin: 0 !important;\n}" >> $out/chrome/userChrome.css
          substituteInPlace $out/chrome/ShyFox/shy-sidebar.css \
            --replace-fail 'content: var(--shyfox-string-open-sidebar);' 'content: none;'
        '';
          in {
            source = "${shyfox}/chrome";
            target = ".mozilla/firefox/ShyFox/chrome/";
            recursive = true;
          };
    };
  };
}
