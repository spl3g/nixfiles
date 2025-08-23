{ config, lib, ... }:
with lib;
let
  cfg = config.nfs;
in
{
  options = {
    nfs.server = mkOption {
      description = ''
          NFS server configuration.
        '';
      default = { enable = false; };
      type = types.submodule {
        options = {
          enable = mkEnableOption "Enable nfs server";
          exportsPath = mkOption {
            type = types.str;
            default = "/export";
            description = ''
          A path to the dir, where exports will be binded.
        '';
          };

          defaultExportIps = mkOption {
            type = types.listOf (types.str);
            description = ''
          A list of ip addresses, that will be used as default in exportDirs
        '';
          };

          defaultExportParams = mkOption {
            type = types.str;
            default = "rw,nohide,insecure,no_subtree_check";
            description = ''
          Params, that will be used as default in exportDirs
        '';
          };

          exportDirs = mkOption {
            description = ''
          A list of directories to export.
        '';
            type = types.listOf (types.submodule {
              options = {
                path = mkOption {
                  type = types.str;
                  description = ''
                A path to the directory to export.
              '';
                };
                exportPath = mkOption {
                  type = types.str;
                  default = "";
                  description = ''
                A path that will be binded to the export directory in the exportsPath.
              '';
                };
                ips = mkOption {
                  type = types.listOf (types.str);
                  default = cfg.server.defaultExportIps;
                  description = ''
                A list of ip addresses to export the dir to.
              '';
                };
                params = mkOption {
                  type = types.str;
                  default = cfg.server.defaultExportParams;
                  description = ''
                Params for the ip addresses.
              '';
                };
              };
            });
          };
        };
      };
    };
  };

  config = mkIf cfg.server.enable {
    services.nfs.server = {
      enable = true;
      exports = "${cfg.server.exportsPath} ${concatMapStrings (ip: "${ip}(rw,fsid=0,no_subtree_check) ") cfg.server.defaultExportIps}\n"
                + concatMapStrings
                  (dir:
                    let
                      ips = concatMapStrings (ip: "${ip}(${dir.params}) ") dir.ips;
                      exportPath = if dir.exportPath != "" then dir.exportPath else
                        baseNameOf dir.path;
                    in "${cfg.server.exportsPath}/${exportPath} ${ips}\n")
                  cfg.server.exportDirs;
    };

    systemd.tmpfiles.rules = [
      "d ${cfg.server.exportsPath} 0744 nobody nogroup"
    ];

    fileSystems = listToAttrs (map (exportDir:
      let
        exportPath = if exportDir.exportPath != "" then exportDir.exportPath else
          baseNameOf exportDir.path;
        fullExportPath = "${cfg.server.exportsPath}/${exportPath}";
      in 
        {
          name = fullExportPath;
          value = {
            device = exportDir.path;
            options = ["bind"];
          };
        }) cfg.server.exportDirs);
  };
}
