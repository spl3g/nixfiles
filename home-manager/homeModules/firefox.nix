{ pkgs, config, lib, inputs, ... }:

{
  options = {
    firefox.enable = lib.mkEnableOption "enable firefox";
  };
  config = lib.mkIf config.firefox.enable {
    programs.firefox = {
      enable = true;
      package = pkgs.firefox;
      profiles.ShyFox = {
        isDefault = true;
        extensions = {
          packages = with pkgs.nur.repos.rycee.firefox-addons; [
            bitwarden
            ublock-origin
            sponsorblock
            return-youtube-dislikes
            firefox-color
            tampermonkey
            duckduckgo-privacy-essentials
            mal-sync
            sidebery
          ];

			    force = true;
          # settings = {
          #   "{3c078156-979c-498b-8990-85f7987dd929}".settings =
          #     builtins.fromJSON (builtins.readFile "${inputs.shimmer.outPath}/sidebery.json");
          # };
        };

        preConfig = builtins.readFile "${inputs.betterfox.outPath}/user.js";
        userChrome = builtins.readFile "${inputs.shimmer.outPath}/userChrome.css";
        userContent = builtins.readFile "${inputs.shimmer.outPath}/userContent.css";
        
        settings = {
          "shimmer.remove-winctr-buttons" = true;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "svg.context-properties.content.enabled" = true;
          "browser.search.suggest.enabled" = true;
          "captivedetect.canonicalURL" = "http://detectportal.firefox.com/canonical.html";
          "network.captive-portal-service.enabled" = true;
          "network.connectivity-service.enabled" = true;
          "extensions.autoDisableScopes" = 0;
        };
        search = {
          force = true;
          engines = {
            "Brave" = {
              urls = [
                { template = "https://search.brave.com/search?q={searchTerms}"; }
                {
                  type = "application/x-suggestions+json";
                  template = "https://search.brave.com/api/suggest?q={searchTerms}";
                }
              ];
              
              icon = "https://cdn.search.brave.com/serp/v2/_app/immutable/assets/safari-pinned-tab.539899c7.svg";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!br" ];
            };
            "NixOS Packages" = {
              urls = [{ template = "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query={searchTerms}"; }];
              icon = "https://nixos.org/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!ns" ];
            };
            "NixOS Options" = {
              urls = [{ template = "https://search.nixos.org/options?channel=unstable&from=0&size=50&sort=relevance&type=packages&query={searchTerms}"; }];
              icon = "https://nixos.org/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!no" ];
            };
            "HomeManager" = {
              urls = [{ template = "https://home-manager-options.extranix.com/?query={searchTerms}&release=master"; }];
              icon = "https://github.com/mipmip/home-manager-option-search/blob/main/images/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!hs" ];
            };
            "NixWiki" = {
              urls = [{ template = "https://wiki.nixos.org/w/index.php?search={searchTerms}"; }];
              icon = "https://nixos.org/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!nw" ];
            };
            "Kinopoisk" = {
              urls = [{ template = "https://www.kinopoisk.ru/index.php?kp_query={searchTerms}"; }];
              icon = "https://www.kinopoisk.ru/favicon.ico";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!ks" ];
            };
            "MDN Docs" = {
              urls = [{ template = "https://developer.mozilla.org/en-US/search?q={searchTerms}"; }];
              icon = "https://developer.mozilla.org/favicon-48x48.bc390275e955dacb2e65.png";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!md" ];
            };
          };
          default = "Brave";
        };
      };
    };
  };
}
