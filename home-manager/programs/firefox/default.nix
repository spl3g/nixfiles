{ pkgs, config, inputs, ... }:
let
  nur = import inputs.nurpkgs {
    inherit pkgs;
    nurpkgs = pkgs;
  };
  extensions = with nur.repos.rycee.firefox-addons; [
    bitwarden
    ghostery
    switchyomega
    sponsorblock
    return-youtube-dislikes
  ];
  userConfig = builtins.readFile ./user.js;
  configOverrides = ''

  '';
  extraConfig = userConfig + configOverrides;
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
    "ai question" = {
      urls = [{ template = "https://iask.ai/?mode=question&q={searchTerms}"; }];
      iconUpdateURL = "https://iask.ai/favicons/favicon-32x32-650bd8771fdea8866630408578e381cc.png?vsn=d";
      updateInterval = 24 * 60 * 60 * 1000;
      definedAliases = [ "!aq" ];
    };
    "ai forums" = {
      urls = [{ template = "https://iask.ai/?mode=forums&q={searchTerms}"; }];
      iconUpdateURL = "https://iask.ai/favicons/favicon-32x32-650bd8771fdea8866630408578e381cc.png?vsn=d";
      updateInterval = 24 * 60 * 60 * 1000;
      definedAliases = [ "!af" ];
    };
    "ai wiki" = {
      urls = [{ template = "https://iask.ai/?mode=wiki&q={searchTerms}"; }];
      iconUpdateURL = "https://iask.ai/favicons/favicon-32x32-650bd8771fdea8866630408578e381cc.png?vsn=d";
      updateInterval = 24 * 60 * 60 * 1000;
      definedAliases = [ "!aw" ];
    };
    "FastGPT" = {
      urls = [{ template = "https://labs.kagi.com/fastgpt?query={searchTerms}"; }];
      definedAliases = [ "!fq" ];
    };
    "NixWiki" = {
      urls = [{ template = "https://nixos.wiki/index.php?search={searchTerms}&go=Go"; }];
      iconUpdateURL = "https://nixos.org/favicon.png";
      updateInterval = 24 * 60 * 60 * 1000;
      definedAliases = [ "!nw" ];
    };
  };
in
{
  programs.firefox = {
    enable = true;
    profiles.Betterfox = {
      isDefault = true;
      inherit extensions extraConfig;
      search = {
        inherit engines;
        default = "Brave";
      };
    };
  };
}
