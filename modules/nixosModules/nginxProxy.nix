{inputs, ...}: {
  flake.nixosModules.nginxProxy = {
    pkgs,
    config,
    lib,
    ...
  }:
    with lib; let
      vhostOptions = import (pkgs.path + "/nixos/modules/services/web-servers/nginx/vhost-options.nix");
      locationOptions = import (pkgs.path + "/nixos/modules/services/web-servers/nginx/location-options.nix");
      nginxOptions = import (pkgs.path + "/nixos/modules/services/web-servers/nginx/default.nix");

      authVhostOptions =
        recursiveUpdate
        (vhostOptions {inherit config lib;})
        {
          options = {
            enableAuthelia = mkOption {
              type = types.bool;
              default = cfg.home.authelia.enable;
            };
          };
        };

      autheliaAuth = ''
        auth_request /internal/authelia/authz;
        auth_request_set $redirection_url $upstream_http_location;
        error_page 401 =302 $redirection_url;

        auth_request_set $user $upstream_http_remote_user;
        auth_request_set $groups $upstream_http_remote_groups;
        auth_request_set $email $upstream_http_remote_email;
        auth_request_set $name $upstream_http_remote_name;

        proxy_set_header Remote-User $user;
        proxy_set_header Remote-Groups $groups;
        proxy_set_header Remote-Email $email;
        proxy_set_header Remote-Name $name;
      '';

      autheliaLocation = url: ''
        internal;
        set $upstream_authelia ${url}/api/authz/auth-request;
        proxy_pass $upstream_authelia;

        ## Headers
        ## The headers starting with X-* are required.
        proxy_set_header X-Original-Method $request_method;
        proxy_set_header X-Original-URL $scheme://$http_host$request_uri;
        proxy_set_header X-Forwarded-For $remote_addr;
        proxy_set_header Content-Length "";
        proxy_set_header Connection "";

        ## Basic Proxy Configuration
        proxy_pass_request_body off;
        proxy_next_upstream error timeout invalid_header http_500 http_502 http_503; # Timeout if the real server is dead
        proxy_redirect http:// $scheme://;
        proxy_http_version 1.1;
        proxy_cache_bypass $cookie_session;
        proxy_no_cache $cookie_session;
        proxy_buffers 4 32k;
        client_body_buffer_size 128k;

        ## Advanced Proxy Configuration
        send_timeout 5m;
        proxy_read_timeout 240;
        proxy_send_timeout 240;
        proxy_connect_timeout 240;
      '';

      cfg = config.nginxProxy;
    in {
      options.nginxProxy = {
        enable = mkEnableOption "Enable nginxProxy";

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
          type = types.attrsOf (types.submodule (locationOptions {inherit config lib;}));
          description = ''
            Subdomains with nginx virtualHosts configuration
          '';
        };

        extraVirtualHosts = mkOption {
          type = types.attrsOf (types.submodule authVhostOptions);
          default = {};
        };

        home = {
          virtualHosts = mkOption {
            type = types.attrsOf (types.submodule authVhostOptions);
            default = {};
            description = ''
              Virtual hosts from another nginx configuration, that will be used to decrypt ssl and forward traffic to another server.
              Make sure that the connection between the two is secure.
            '';
          };

          subdomains = mkOption {
            type = types.attrsOf (types.submodule (locationOptions {inherit config lib;}));
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

          authelia = mkOption {
            type = types.submodule {
              options = {
                enable = mkOption {
                  type = types.bool;
                  default = true;
                };
                publicUrl = mkOption {
                  type = types.str;
                  default = "https://auth.${cfg.domain}/";
                };
                localUrl = mkOption {
                  type = types.str;
                  default = "http://127.0.0.1:9091";
                };
              };
            };
            default = {};
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
          extraGroups = ["acme"];
          isSystemUser = true;
        };
        services.nginx = let
          ssl = {
            forceSSL = cfg.acme.enable;
            enableACME = cfg.acme.enable;
          };

          makeVhosts = domain: subdomains:
            lib.concatMapAttrs
            (name: value: {${name + "." + domain} = {locations."/" = value;} // ssl;})
            subdomains;

          homeRoutes = homeVirtualHosts: homeUrl:
            builtins.mapAttrs
            (name: value:
              recursiveUpdate value {
                locations."/" = {
                  proxyPass = homeUrl;
                  recommendedProxySettings = true;
                };
              }
              // ssl)
            homeVirtualHosts;

          removeAuthelia = filterAttrsRecursive (n: v: n != "enableAuthelia");

          vhosts = makeVhosts cfg.domain cfg.subdomains;
          homeVhosts = homeRoutes (recursiveUpdate (makeVhosts (cfg.home.domain) cfg.home.subdomains) cfg.home.virtualHosts) cfg.home.url;
          addAutheliaRoutes = isHome: vhosts:
            builtins.mapAttrs
            (name: value: (recursiveUpdate value {
              locations."/" = {
                extraConfig =
                  value.locations."/".extraConfig or ""
                  + concatStrings (optional (value.enableAuthelia or true && !isHome) autheliaAuth);
              };
              locations."/internal/authelia/authz" = mkIf (value.enableAuthelia or true && !isHome) {
                extraConfig = autheliaLocation cfg.home.authelia.localUrl;
              };
            }))
            vhosts;
        in
          {
            enable = true;
            recommendedProxySettings = cfg.recommendedProxySettings;

            virtualHosts =
              removeAuthelia
              (addAutheliaRoutes
                (homeVhosts == {})
                (recursiveUpdate (recursiveUpdate vhosts homeVhosts) cfg.extraVirtualHosts));
          }
          // cfg.extraConfig;
      };
    };
}
