{
  inputs,
  self,
  ...
}: {
  flake.nixosModules.booklore = {
    config,
    lib,
    pkgs,
    ...
  }:
    with lib; let
      cfg = config.services.booklore;
    in {
      options = {
        services.booklore = {
          enable = mkEnableOption "Enable booklore service";
          subdomain = mkOption {
            type = types.str;
            description = ''
              Subdomain to use for nginx.
            '';
          };
          uid = mkOption {
            type = types.str;
            description = ''
              UID for the container user.
            '';
          };
          gid = mkOption {
            type = types.str;
            description = ''
              GID for the container user.
            '';
          };
          settings = {
            timezone = mkOption {
              type = types.str;
              description = ''
                Timezone string;
              '';
            };
            dataDir = mkOption {
              type = types.path;
              description = ''
                Booklore data directory.
              '';
              default = "/var/lib/booklore";
            };
            bookdropDir = mkOption {
              type = types.path;
              description = ''
                Directory where booklore will injest books. It is not created automatically, you should create it with the sufficient permissions for the uid and gid you provided.
              '';
            };
            booksDir = mkOption {
              type = types.path;
              description = ''
                Directory where booklore will store books. It is not created automatically, you should create it with the sufficient permissions for the uid and gid you provided.
              '';
            };
          };
          database = {
            name = mkOption {
              type = types.str;
              default = "booklore";
            };
            user = mkOption {
              type = types.str;
              default = "files";
            };
            password = mkOption {
              type = types.str;
              default = "booklore";
            };
          };
        };
      };

      config = mkIf cfg.enable {
        createPaths = {
          "${cfg.settings.dataDir}" = {
            owner = cfg.uid;
            group = cfg.gid;
            permissions = "0750";
          };
        };
        nginxProxy = {
          enable = true;
          subdomains = {
            "${cfg.subdomain}" = {
              proxyPass = "http://127.0.0.1:6060";
              proxyWebsockets = true;
            };
          };
        };

        virtualisation.oci-containers.containers.booklore = {
          image = "grimmory/grimmory:latest";
          environment = {
            USER_ID = cfg.uid;
            GROUP_ID = cfg.gid;
            TZ = cfg.settings.timezone;

            DATABASE_URL = "jdbc:mariadb://mariadb-booklore:3306/${cfg.database.name}";
            DATABASE_USERNAME = cfg.database.user;
            DATABASE_PASSWORD = cfg.database.password;
          };
          ports = [
            "127.0.0.1:6060:6060"
          ];
          volumes = [
            "${cfg.settings.dataDir}:/app/data"
            "${cfg.settings.booksDir}:/books"
            "${cfg.settings.bookdropDir}:/bookdrop"
          ];
          dependsOn = [
            "mariadb-booklore"
          ];
          networks = [
            "booklore_default"
          ];
        };
        systemd.services."podman-booklore" = {
          serviceConfig = {
            Restart = lib.mkOverride 90 "always";
          };
          after = [
            "podman-network-booklore_default.service"
          ];
          requires = [
            "podman-network-booklore_default.service"
          ];
        };

        virtualisation.oci-containers.containers.mariadb-booklore = {
          image = "lscr.io/linuxserver/mariadb:11.4.5";
          environment = {
            PUID = "1000";
            PGID = "1000";
            TZ = cfg.settings.timezone;
            MYSQL_DATABASE = cfg.database.name;
            MYSQL_USER = cfg.database.user;
            MYSQL_PASSWORD = cfg.database.password;
          };
          networks = [
            "booklore_default"
          ];
        };

        systemd.services."podman-mariadb-booklore" = {
          serviceConfig = {
            Restart = lib.mkOverride 90 "always";
          };
          after = [
            "podman-network-booklore_default.service"
          ];
          requires = [
            "podman-network-booklore_default.service"
          ];
        };

        systemd.services."podman-network-booklore_default" = {
          path = [pkgs.podman];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStop = "podman network rm -f booklore_default";
          };
          script = ''
            podman network inspect booklore_default || podman network create booklore_default
          '';
        };
      };
    };
}
