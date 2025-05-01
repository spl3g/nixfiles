{ pkgs, lib, config, inputs, ... }:

{
  imports = [
    ./waybar.nix
    ./rofi.nix
    ./mako.nix
  ];
  
  options = {
    hyprland.enable = lib.mkEnableOption "enable hyprland";
  };
  
  config = lib.mkIf config.hyprland.enable {
    waybar.enable = true;
    rofi.enable = true;
    mako.enable = true;

    home.packages = with pkgs; [
      swww
      brightnessctl
      grimblast
      cliphist
      polkit_gnome
      kdePackages.xwaylandvideobridge
      wl-clipboard
      libnotify
      socat
    ];

    home.sessionVariables.XDG_CURRENT_DESKTOP = "Hyprland";

    wayland.windowManager.hyprland = {
      enable = true;
      settings = {
        "$scripts" = "${./attachments/hypr-scripts}";
        "$mainMod" = "SUPER";
        "$terminal" = "alacritty";
        
        exec-once = [
          "emacs --daemon"
          "swww-daemon"
          "swww img ${config.wallpaper}"
          "$scripts/bitwarden-float.sh"
        ];
        
        monitor = ",preferred,auto,1,mirror,eDP-1";

        input = {
          kb_layout = "us,ru";
          kb_options = "grp:win_space_toggle";
          touchpad = {
            natural_scroll = "yes";
            disable_while_typing = "no";
          };
          sensitivity = 0.1;
        };
        
        general = {
          gaps_in = 5;
          gaps_out = 20;
          border_size = 3;
          layout = "dwindle";
          allow_tearing = false;
        };

        decoration = {
          rounding = 7;
          blur = {
            enabled = true;
            size = 3;
            passes = 1;
          };
        };

        animations = {
          enabled = 1;
          bezier = "overshot,0.13,0.99,0.29,1.1,";
          animation = [
            "fade,1,4,default"
            "workspaces,1,4,default,fade"
            "windows,1,4,overshot,popin 95%"
          ];
        };

        dwindle = {
          pseudotile = "yes";
          preserve_split = "yes";
        };

        gestures.workspace_swipe = "on";
        misc.force_default_wallpaper = 1;

        windowrulev2 = [
          "float, title:^(Firrfox — Sharing Indicator)$"
          "noborder, title:^(Firefox — Sharing Indicator)$"
          "rounding 0, title:^(Firefox — Sharing Indicator)$"
          "float, title:^(Picture-in-Picture)$"
          "pin, title:^(Picture-in-Picture)$"
          "move 100%-w-20 100%-w-20, title:^(Picture-in-Picture)$"
          "noinitialfocus, title:^(Picture-in-Picture)$"
          "float, title:^(Save File)$"
          "pin, title:^(Save File)$"
          "pin, class:^(dragon-drop)$"
          "float, title:^(Torrent Options)$"
          "pin, title:^(Torrent Options)$"
          "opacity 0.0 override 0.0 override,class:^(xwaylandvideobridge)$"
          "noanim,class:^(xwaylandvideobridge)$"
          "noinitialfocus,class:^(xwaylandvideobridge)$"
          "maxsize 1 1,class:^(xwaylandvideobridge)$"
          "noblur,class:^(xwaylandvideobridge)$"
        ];

        bind = [
          "$mainMod, V, togglefloating, "
          "$mainMod, P, pseudo,"
          "$mainMod, I, togglesplit,"
          "$mainMod, F, fullscreen, 0"
          "$mainMod, M, fullscreen, 1"
          "$mainMod SHIFT, Q, killactive, "
          "$mainMod SHIFT, E, exit,"
          
          # Apps
          "$mainMod, D, exec, killall rofi || rofi -show-icons -show drun"
          "$mainMod, Q, exec, $terminal"
          "$mainMod, B, exec, zen-beta"
          "$mainMod, T, exec, telegram-desktop"
          "$mainMod, E, exec, emacsclient -c -a emacs"
          "$mainMod CONTROL, E, exec, emacs"
          "$mainMod SHIFT, Esc, exec, swww img ${config.wallpaper}"
          ",XF86HangupPhone, exec, bash $scripts/toggle-vpn.sh"
          
          # Screenshooting
          ", Print, exec, grimblast save screen"
          "ALT, Print, exec, grimblast save active"
          "SHIFT, Print, exec, grimblast save area"
          "CONTROL, Print, exec, grimblast copy screen"
          "ALT_CONTROL, Print, exec, grimblast copy active"
          "CONTROL_SHIFT, Print, exec, grimblast copy area "

          # Volume
          ",0x1008FF11,exec,wpctl set-volume @DEFAULT_SINK@ 5%-"
          ",0x1008FF13,exec,wpctl set-volume @DEFAULT_SINK@ 5%+"
          ",0x1008FF12,exec,wpctl set-mute @DEFAULT_SINK@ toggle"
          ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_SOURCE@ toggle"
          ",XF86PickupPhone, exec, python3 $scripts/switch-sink.py"

          # Brightness
          ",XF86MonBrightnessUp,exec,brightnessctl s +5%"
          ",XF86MonBrightnessDown,exec,brightnessctl s 5%-"

          # Windows
          "$mainMod, J, movefocus, d"
          "$mainMod, K, movefocus, u"
          "$mainMod, H, movefocus, l"
          "$mainMod, L, movefocus, r"
          "SUPER_SHIFT,J,movewindow,d"
          "SUPER_SHIFT,K,movewindow,u"
          "SUPER_SHIFT,H,movewindow,l"
          "SUPER_SHIFT,L,movewindow,r"
          "$mainMod, S, togglespecialworkspace, magic"
          "$mainMod SHIFT, S, movetoworkspace, special:magic"
          "$mainMod, mouse_down, workspace, e+1"
          "$mainMod, mouse_up, workspace, e-1"
        ] ++ (
          # workspaces
          # binds $mod + [shift +] {1..10} to [move to] workspace {1..10}
          builtins.concatLists (builtins.genList (
            x: let
              ws = let
                c = (x + 1) / 10;
              in
                builtins.toString (x + 1 - (c * 10));
            in [
              "$mainMod, ${ws}, workspace, ${toString (x + 1)}"
              "$mainMod SHIFT, ${ws}, movetoworkspacesilent, ${toString (x + 1)}"
            ]
          )
            10)
        );
        
        bindm = [
          "$mainMod, mouse:272, movewindow"
          "$mainMod, mouse:273, resizewindow"
        ];
      };
    };
  };
}
