{ pkgs, config, lib, ... }:
with lib;
let
  vhostOptions = import (pkgs.path + "/nixos/modules/services/web-servers/nginx/vhost-options.nix");
  locationOptions = import (pkgs.path + "/nixos/modules/services/web-servers/nginx/location-options.nix");
  nginxOptions = import (pkgs.path + "/nixos/modules/services/web-servers/nginx/default.nix");
  cfg = config.nginx;
in
{
  options.nginx = {
    enable = mkEnableOption "Enable nginx";

    domain = mkOption {
      type = types.str;
      description = ''
        Domain to use with subdomains
      '';
    };

    recommendedProxySettings = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Enables global recommended proxy settings
      '';
    };

    subdomains = mkOption {
      type = types.attrsOf (types.submodule (locationOptions { inherit config lib; }));
      description = ''
        Subdomains with nginx virtualHosts configuration
      '';
    };

    extraVirtualHosts = mkOption {
      type = types.attrsOf (types.submodule (vhostOptions { inherit config lib; }));
      default = {};
    };

    home = {
      virtualHosts = mkOption {
        type = types.attrsOf (types.submodule (vhostOptions { inherit config lib; }));
        default = {};
        description = ''
          Virtual hosts from another nginx configuration, that will be used to decrypt ssl and forward traffic to another server.
          Make sure that the connection between the two is secure.
        '';
      };

      subdomains = mkOption {
        type = types.attrsOf (types.submodule (locationOptions { inherit config lib; }));
        default = {};
        description = ''
          Subdomains from another nginx configuration, that will be used to decrypt ssl and forward traffic to another server.
          Make sure that the connection between the two is secure.
        '';
      };

      domain = mkOption {
        type = types.str;
        default = cfg.domain;
        description = ''
          Home domain, if no domain provided, the current will be used;
        '';
      };
      
      url = mkOption {
        type = types.str;
        default = "";
        description = ''
          Url that requests would be passed to; 
        '';
      };
    };

    acme = {
      enable = mkEnableOption "enable acme certs";
      email = mkOption {
        type = types.str;
        default = "notspl3g+acme@duck.com";
      };
    };

    extraConfig = mkOption {
      type = types.attrsOf (types.submodule nginxOptions);
      default = {};
      description = ''
        Extra nginx config.
      '';
    };
  };
  
  config = mkIf cfg.enable {
    security.acme = mkIf cfg.acme.enable {
      acceptTerms = true;
      defaults.email = cfg.acme.email;
    };

    users.groups.nginx = mkIf cfg.acme.enable {};
    users.users.nginx = mkIf cfg.acme.enable {
      group = "nginx";
      extraGroups = [ "acme" ];
      isSystemUser = true;
    };
    services.nginx =
      let
        ssl = {
          forceSSL = cfg.acme.enable;
          enableACME = cfg.acme.enable;
        };

        makeVhosts = (domain: subdomains:
          lib.concatMapAttrs
            (name: value: { ${name + "." + domain} = { locations."/" = value; } // ssl; })
            subdomains);

        homeRoutes = (homeVirtualHosts: homeUrl:
          builtins.mapAttrs
            (name: value: {
              locations."/" = value.locations."/" // {
                proxyPass = homeUrl;
                recommendedProxySettings = true;
              };
            } // ssl)
            homeVirtualHosts);

        vhosts = (makeVhosts cfg.domain cfg.subdomains);
        homeVhosts = (homeRoutes ((makeVhosts (cfg.home.domain) cfg.home.subdomains) // cfg.home.virtualHosts) cfg.home.url);
      in {
        enable = true;
        recommendedProxySettings = cfg.recommendedProxySettings;
        
        virtualHosts =  vhosts // homeVhosts // cfg.extraVirtualHosts;
      } // cfg.extraConfig;
  };
}
