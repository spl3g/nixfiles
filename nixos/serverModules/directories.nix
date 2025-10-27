{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.createPaths;
  pathAttrsToListRec = pathsAttrSet: parentPath: parentConfig:
    lib.flatten (lib.mapAttrsToList (path: config: let
      filteredConfig = lib.filterAttrs (n: v: v != null) (builtins.removeAttrs config ["subPaths"]);
      out =
        {
          path =
            if parentPath == ""
            then path
            else parentPath + "/" + path;
        }
        // parentConfig // filteredConfig;
    in
      if config ? subPaths
      then [out] ++ (pathAttrsToListRec config.subPaths path filteredConfig)
      else [out])
    pathsAttrSet);
  pathConfig = {
    options = {
      group = mkOption {
        type = types.nullOr types.str;
        default = null;
      };
      owner = mkOption {
        type = types.nullOr types.str;
        default = null;
      };
      permissions = mkOption {
        type = types.nullOr types.str;
        default = null;
      };
      subPaths = mkOption {
        type = types.attrsOf (types.submodule pathConfig);
        default = {};
      };
    };
  };
  pathList = pathAttrsToListRec cfg "" {};
in rec {
  options = {
    createPaths = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          group = mkOption {
            type = types.nullOr types.str;
            default = null;
          };
          owner = mkOption {
            type = types.nullOr types.str;
            default = null;
          };
          permissions = mkOption {
            type = types.nullOr types.str;
            default = null;
          };
          subPaths = mkOption {
            type = types.attrsOf (types.submodule pathConfig);
            default = {};
            description = ''
              SubPaths to create using systemd tmpfiles.
            '';
          };
        };
      });
      default = {};
      description = ''
        Paths to create using systemd tmpfiles.
      '';
    };
  };

  config = mkIf (cfg != {}) {
    systemd.tmpfiles.rules =
      map
      (dir: "d ${dir.path} ${dir.permissions} ${dir.owner} ${dir.group}")
      pathList;

    users = let
      extraGroups = map (path: path.group) pathList;
    in {
      groups = genAttrs extraGroups (group: {});
    };
  };
}
