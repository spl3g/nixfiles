{ pkgs, config, lib, ... }:

{
  imports = [
    ./picom.nix
    ./dunst.nix
    ./sxhkd.nix
  ];
  
  options = {
    bspwm.enable = lib.mkEnableOption "enable bspwm";
  };

  config = lib.mkIf config.bspwm.enable {
    picom.enable = true;
    dunst.enable = true;
    sxhkd.enable = true;

    rofi = {
      enable = true;
      package = pkgs.rofi;
    };
    
    xsession.windowManager.bspwm = {
      enable = true;
      
      monitors =
        let
          workspaces = [
            "α"
            "β"
            "γ"
            "δ"
            "ε"
          ];
        in {
          "^1" = workspaces;
          # "^2" = workspaces;
        };
      
      settings = {
        # focused_border_color = "#908caa";
        # normal_border_color = "#363a4f";
        # presel_feedback_color = "#752f20";
        border_width = 3;
        window_gap = 12;
        focus_follows_pointer = true;
        split_ratio = 0.5;
      };
      
      startupPrograms = [
        "sxhkd"
        "picom -b"
        "emacs --daemon"
        "feh --bg-fill ${config.wallpaper}"
      ];
    };
    home.packages = with pkgs; [
      feh
      betterlockscreen
      xfce.xfce4-screenshooter
    ];
  };
}
