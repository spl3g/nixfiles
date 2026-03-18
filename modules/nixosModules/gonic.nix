{inputs, ...}: {
  flake.nixosModules.gonic = {
    config,
    lib,
    pkgs,
    ...
  }:
    with lib; let
      cfg = config.gonic;
    in {
      options = {
        gonic = {
          enable = mkEnableOption "enable gonic configuration";

          listenAddr = mkOption {
            type = types.str;
            default = "127.0.0.1:4747";
            description = ''
              Address that gonic will listen on.
            '';
          };

          extraGroups = mkOption {
            type = types.listOf (types.str);
            default = [];
            description = ''
              Additional groups for gonic.
            '';
          };

          musicPaths = mkOption {
            type = types.listOf (types.str);
            description = ''
              Directories with music in it.
            '';
          };

          podcastsPath = mkOption {
            type = types.str;
            default = "${cfg.stateDir}/podcasts";
            description = ''
              Directory for podcasts.
            '';
          };

          playlistsPath = mkOption {
            type = types.str;
            default = "${cfg.stateDir}/playlists";
            description = ''
              Directory for playlists.
            '';
          };

          stateDir = mkOption {
            type = types.str;
            default = "/var/lib/gonic";
            description = ''
              A directory where gonic will keep their files.
            '';
          };

          settings = mkOption {
            default = {};
            description = ''
              Additional gonic settings
            '';
          };
        };
      };

      config = mkIf cfg.enable {
        systemd.services.gonic.serviceConfig = {
          DynamicUser = lib.mkForce false;
          User = "gonic";
          Group = "gonic";
          SupplementaryGroups = cfg.extraGroups;
          ReadWritePaths = [
            cfg.podcastsPath
            cfg.playlistsPath
          ];
        };

        users = {
          groups = {
            gonic = {};
          };

          users.gonic = {
            isSystemUser = true;
            group = "gonic";
          };
        };

        systemd.tmpfiles.rules = [
          "d ${cfg.stateDir} 0755 gonic gonic"
          "d ${cfg.podcastsPath} 0755 gonic gonic"
          "d ${cfg.playlistsPath} 0755 gonic gonic"
        ];

        services.gonic = {
          enable = true;
          settings =
            {
              listen-addr = cfg.listenAddr;
              music-path = cfg.musicPaths;
              playlists-path = [cfg.playlistsPath];
              podcast-path = [cfg.podcastsPath];
              db-path = ["${cfg.stateDir}/gonic.db"];
            }
            // cfg.settings;
        };
      };
    };
}
