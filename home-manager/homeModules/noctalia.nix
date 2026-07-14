{
  pkgs,
  config,
  lib,
  inputs,
  ...
}:
with lib; let
  cfg = config.noctalia;
in {
  imports = [
    inputs.noctalia.homeModules.default
  ];

  options = {
    noctalia = {
      enable = mkEnableOption "enable noctalia config";
    };
  };

  config = mkIf cfg.enable {
    programs.noctalia = {
      enable = true;
      settings = {
        bar = {
          order = ["main"];
          main = {
            enabled = true;
            position = "left";

            border = "outline";
            border_width = 2;
            shadow = false;

            margin_ends = 10;
            widget_spacing = 12;
            thickness = 40;

            start = ["workspaces" "group:sysmon"];
            center = ["clock"];
            end = ["tray" "network" "bluetooth" "volume" "brightness" "battery" "notifications" "control-center"];
            capsule_group = [
              {
                id = "sysmon";
                members = ["cpu" "ram" "temp"];
                padding = 6.0;
              }
            ];
          };
        };

        widget = {
          cpu = {
            type = "sysmon";
            stat = "cpu_usage";
            show_label = false;
          };

          temp = {
            type = "sysmon";
            stat = "cpu_temp";
            show_label = false;
          };

          ram = {
            type = "sysmon";
            stat = "ram_used";
            show_label = false;
          };

          tray = {
            drawer = true;
          };
        };

        notifications = {
          enable_daemon = true;
        };

        wallpaper = {
          enabled = true;
          path = config.wallpaper;
        };

        keybinds = {
          down = ["Ctrl+n"];
          left = ["Ctrl+b"];
          right = ["Ctrl+f"];
          up = ["Ctrl+p"];
        };
      };
    };
  };
}
