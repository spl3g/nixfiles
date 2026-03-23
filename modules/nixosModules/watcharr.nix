{
  inputs,
  self,
  ...
}: {
  flake.nixosModules.watcharr = {
    config,
    lib,
    pkgs,
    ...
  }:
    with lib; let
      cfg = config.services.watcharr;
      port = builtins.toString cfg.settings.port;
    in {
      options = {
        services.watcharr = {
          enable = mkEnableOption "Enable watcharr service";
          subdomain = mkOption {
            type = types.str;
            description = ''
              Subdomain to use for nginx.
            '';
          };
          settings = {
            dataDir = mkOption {
              type = types.path;
              description = ''
                Watcharr data directory.
              '';
              default = "/var/lib/watcharr";
            };
            port = mkOption {
              type = types.port;
              default = 3080;
              description = ''
                Port to use.
              '';
            };
          };
        };
      };

      config = mkIf cfg.enable {
        createPaths = {
          "${cfg.settings.dataDir}" = {
            owner = "root";
            group = "root";
            permissions = "0750";
          };
        };

        nginxProxy = {
          enable = true;
          subdomains = {
            "${cfg.subdomain}" = {
              proxyPass = "http://127.0.0.1:${port}";
              proxyWebsockets = true;
            };
          };
        };

        virtualisation.oci-containers.containers.watcharr = {
          image = "ghcr.io/sbondco/watcharr:latest";
          ports = [
            "127.0.0.1:${port}:3080"
          ];
          volumes = [
            "${cfg.settings.dataDir}:/data"
          ];
        };
      };
    };
}
