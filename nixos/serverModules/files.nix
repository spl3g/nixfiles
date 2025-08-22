{ config, lib, ... }:
with lib;
let
  cfg = config.filesDir;
in
{
  options = {
    filesDir = {
      enable = mkEnableOption "Enable the creation of a main files directory and nfs binds for it.";
      mainDir = mkOption {
        type = types.str;
        default = "/srv/files";
        description = ''
          The main file dir.
        '';
      };
      subPaths = mkOption {
        type = types.listOf (types.submodule {
          options = {
            path = mkOption {
              type = types.str;
            };

            group = mkOption {
              type = types.str;
            };
          };
        });
        default = [];
        description = ''
          Subpaths to create under the files dir.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.tmpfiles.rules = [
      "d ${cfg.mainDir} 0770 files files"
    ] ++ (map (dir: "d ${cfg.mainDir}/${dir.path} 0770 files ${dir.group}") cfg.subPaths);

    users =
      let
        extraGroups = (map (dir: dir.group) cfg.subPaths);
      in {
        groups = {
          files = {};
        } // genAttrs extraGroups (group: {});

        users.files = {
          isNormalUser = true;
          group = "files";
          home = cfg.mainDir;
          homeMode = "770";
          inherit extraGroups;
        };
      };
  };
} 
