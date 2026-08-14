{inputs, ...}: {
  flake.nixosModules.omnisearch = {
    config,
    lib,
    pkgs,
    ...
  }: let
    cfg = config.services.omnisearch;
    pkg = cfg.package;

    settingsFormat = pkgs.formats.ini {};
  in {
    options.services.omnisearch = {
      enable = lib.mkEnableOption "OmniSearch metasearch engine";

      package = lib.mkOption {
        type = lib.types.package;
        default = inputs.omnisearch.packages.${pkgs.stdenv.hostPlatform.system}.default;
        description = "The omnisearch package to use.";
      };

      configFile = lib.mkOption {
        type = lib.types.path;
        default = settingsFormat.generate "omnisearch-config.ini" cfg.settings;
        description = "Path to a custom config.ini. Overrides 'settings'.";
      };

      settings = lib.mkOption {
        description = "omnisearch configuration, see the example-config.ini in the repo.";
        type = settingsFormat.type;
        default = {
          server = {
            host = "0.0.0.0";
            port = 8087;
            domain = "http://localhost:${cfg.settings.port}";
          };
          engines = {
            engines = "*";
          };
        };
      };
    };

    config = lib.mkIf cfg.enable {
      systemd.services.omnisearch = {
        description = "OmniSearch Service";
        after = ["network.target"];
        wantedBy = ["multi-user.target"];

        serviceConfig = {
          ExecStart = "${pkg}/bin/omnisearch";

          WorkingDirectory = "/var/lib/omnisearch";
          StateDirectory = "omnisearch";
          CacheDirectory = "omnisearch";

          BindReadOnlyPaths = [
            "${pkg}/share/omnisearch/templates:/var/lib/omnisearch/templates"
            "${pkg}/share/omnisearch/static:/var/lib/omnisearch/static"
            "${pkg}/share/omnisearch/locales:/var/lib/omnisearch/locales"
            "${cfg.configFile}:/var/lib/omnisearch/config.ini"
          ];

          DynamicUser = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          PrivateTmp = true;
          Restart = "always";
        };
      };
    };
  };
}
