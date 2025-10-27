{
  pkgs,
  config,
  lib,
  inputs,
  ...
}:
with lib; let
  cfg = config.niri;
in {
  imports = [
    ./waybar.nix
    ./rofi.nix
    ./mako.nix
    inputs.niri.homeModules.niri
    inputs.niri.homeModules.stylix
  ];

  options = {
    niri = {
      enable = mkEnableOption "enable niri config";
    };
  };

  config = mkIf cfg.enable {
    waybar = {
      enable = true;
      windowManager = "niri";
      workspaceIcons = {
        # "1" = "α";
        # "2" = "β";
        # "3" = "γ";
        # "4" = "δ";
        # "5" = "ε";
        # urgent = "λ";
        # default = "ω";
        "internet" = "";
        "discord" = "";
        "chat" = "<b></b>";

        "active" = "";
        "default" = "";
      };
    };
    rofi.enable = true;
    mako.enable = true;

    home.packages = with pkgs; [
      pkgs.xwayland-satellite
      swww
      brightnessctl
      grimblast
      polkit_gnome
      kdePackages.xwaylandvideobridge
      wl-clipboard
      libnotify
    ];

    stylix.targets.niri.enable = true;
    programs.niri = {
      enable = true;

      settings = {
        input = {
          keyboard.xkb = {
            layout = "us,ru";
            options = "grp:win_space_toggle,compose:ralt,ctrl:nocaps";
          };
          touchpad = {
            tap = true;
            dwt = false;
            dwtp = true;
            natural-scroll = true;
          };
          warp-mouse-to-focus.enable = true;
          focus-follows-mouse = {
            enable = true;
            max-scroll-amount = "25%";
          };
        };

        cursor = {
          theme = "Bibata-Modern-Ice";
          size = 24;
        };

        layout = {
          gaps = 16;
          center-focused-column = "never";
          preset-column-widths = [
            {proportion = 0.33333;}
            {proportion = 0.5;}
            {proportion = 0.66667;}
          ];
          border = {
            enable = true;
          };
          focus-ring.enable = false;
        };

        animations = {
          workspace-switch.enable = false;
        };

        # workspaces = {
        #   internet = {};
        #   code = {};
        # };

        window-rules = [
          {
            matches = [
              {
                app-id = "steam";
                title = ''r#"^notificationtoasts_\d+_desktop$"#'';
              }
            ];
            default-floating-position = {
              x = 10;
              y = 10;
              relative-to = "bottom-right";
            };
          }
        ];

        spawn-at-startup = [
          {argv = ["waybar"];}
          {argv = ["swww-daemon"];}
          {argv = ["mako"];}
        ];

        prefer-no-csd = true;

        binds = with config.lib.niri.actions; let
          scripts = "${./attachments/hypr-scripts}";
        in {
          "Mod+Q".action.spawn = "alacritty";
          "Mod+D".action.spawn = ["sh" "-c" "pkill rofi || rofi -show-icons -show drun"];
          "Mod+B".action.spawn = "zen-beta";
          "Mod+E".action.spawn = ["emacsclient" "-c" "-a" "emacs"];
          "Mod+T".action.spawn = "Telegram";

          "XF86AudioRaiseVolume".action.spawn = ["wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.05+"];
          "XF86AudioLowerVolume".action.spawn = ["wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.05-"];
          "XF86AudioMute".action.spawn = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"];
          "XF86AudioMicMute".action.spawn = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"];

          "XF86MonBrightnessUp".action.spawn = ["brightnessctl" "s" "+5%"];
          "XF86MonBrightnessDown".action.spawn = ["brightnessctl" "s" "5%-"];

          "XF86Favorites".action.spawn = "${scripts}/toggle-vpn.sh";
          "XF86TouchpadToggle".action.spawn = "${scripts}/switch-sink.py";

          "Mod+Shift+Slash".action = show-hotkey-overlay;

          "Super+Alt+L".action.spawn = "swaylock";
          "Mod+Shift+E".action = quit;
          "Mod+Shift+P".action = power-off-monitors;
          "Mod+Shift+Q".action = close-window;

          "Mod+H".action = focus-column-left;
          "Mod+J".action = focus-window-down;
          "Mod+K".action = focus-window-up;
          "Mod+L".action = focus-column-right;

          "Mod+Shift+H".action = move-column-left;
          "Mod+Shift+J".action = move-window-down;
          "Mod+Shift+K".action = move-window-up;
          "Mod+Shift+L".action = move-column-right;

          "Mod+Home".action = focus-column-first;
          "Mod+End".action = focus-column-last;

          "Mod+Ctrl+Home".action = move-column-to-first;
          "Mod+Ctrl+End".action = move-column-to-last;

          "Mod+Ctrl+H".action = focus-monitor-left;
          "Mod+Ctrl+J".action = focus-monitor-down;
          "Mod+Ctrl+K".action = focus-monitor-up;
          "Mod+Ctrl+L".action = focus-monitor-right;

          "Mod+Shift+Ctrl+H".action = move-column-to-monitor-left;
          "Mod+Shift+Ctrl+J".action = move-column-to-monitor-down;
          "Mod+Shift+Ctrl+K".action = move-column-to-monitor-up;
          "Mod+Shift+Ctrl+L".action = move-column-to-monitor-right;

          "Mod+U".action = focus-workspace-down;
          "Mod+I".action = focus-workspace-up;

          "Mod+Ctrl+U".action = move-column-to-workspace-down;
          "Mod+Ctrl+I".action = move-column-to-workspace-up;

          "Mod+Shift+U".action = move-workspace-down;
          "Mod+Shift+I".action = move-workspace-down;

          "Mod+WheelScrollRight".action = focus-column-right;
          "Mod+WheelScrollLeft".action = focus-column-left;

          "Mod+Ctrl+WheelScrollRight".action = move-column-right;
          "Mod+Ctrl+WheelScrollLeft".action = move-column-left;

          "Mod+Shift+WheelScrollDown".action = focus-column-right;
          "Mod+Shift+WheelScrollUp".action = focus-column-left;

          "Mod+Ctrl+Shift+WheelScrollDown".action = move-column-right;
          "Mod+Ctrl+Shift+WheelScrollUp".action = move-column-left;

          "Mod+1".action.focus-workspace = 1;
          "Mod+2".action.focus-workspace = 2;
          "Mod+3".action.focus-workspace = 3;
          "Mod+4".action.focus-workspace = 4;
          "Mod+5".action.focus-workspace = 5;
          "Mod+6".action.focus-workspace = 6;
          "Mod+7".action.focus-workspace = 7;
          "Mod+8".action.focus-workspace = 8;
          "Mod+9".action.focus-workspace = 9;

          "Mod+Shift+1".action.move-column-to-workspace = 1;
          "Mod+Shift+2".action.move-column-to-workspace = 2;
          "Mod+Shift+3".action.move-column-to-workspace = 3;
          "Mod+Shift+4".action.move-column-to-workspace = 4;
          "Mod+Shift+5".action.move-column-to-workspace = 5;
          "Mod+Shift+6".action.move-column-to-workspace = 6;
          "Mod+Shift+7".action.move-column-to-workspace = 7;
          "Mod+Shift+8".action.move-column-to-workspace = 8;
          "Mod+Shift+9".action.move-column-to-workspace = 9;

          "Mod+Comma".action = consume-window-into-column;
          "Mod+Period".action = expel-window-from-column;
          "Mod+BracketLeft".action = consume-or-expel-window-left;
          "Mod+BracketRight".action = consume-or-expel-window-right;

          "Mod+M".action = maximize-column;
          "Mod+F".action = fullscreen-window;
          "Mod+C".action = center-column;

          "Mod+R".action = switch-preset-column-width;
          "Mod+Minus".action.set-column-width = "-10%";
          "Mod+Equal".action.set-column-width = "+10%";
          "Mod+Shift+Minus".action.set-window-height = "-10%";
          "Mod+Shift+Equal".action.set-window-height = "+10%";

          "Mod+V".action = toggle-window-floating;

          "Print".action.screenshot.show-pointer = true;
          "Shift+Print".action.screenshot-screen.write-to-disk = true;
          "Ctrl+Shift+Print".action.screenshot-screen.write-to-disk = false;
          "Alt+Print".action.screenshot-window.write-to-disk = true;
          "Ctrl+Alt+Print".action.screenshot-window.write-to-disk = false;
        };
      };
    };
  };
}
