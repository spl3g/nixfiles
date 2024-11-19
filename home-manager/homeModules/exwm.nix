{ lib, pkgs, config, ... }:
{
  imports = [
    ./picom.nix
    ./dunst.nix
  ];

  options = {
    exwm.enable = lib.mkEnableOption " enable exwm";
  };
  
  config = lib.mkIf config.exwm.enable {
    picom.enable = true;
    dunst.enable = true;
    
    programs.emacs = {
      extraPackages = epkgs: with epkgs; [ exwm ];
      extraConfig = ''
        (setq exwm--my-scripts "${./attachments/hypr-scripts}")
      '';
    };
    services = {
      # Screenshotting.
      flameshot.enable = true;

      # Screen locking.
      screen-locker = {
        enable = true;
        lockCmd = "${pkgs.i3lock-fancy}/bin/i3lock-fancy -p -t ''";
        inactiveInterval = 20;
      };
    };
    home.file.xinitrc = {
      text = ''
        # Disable access control for the current user.
        xhost +SI:localuser:$USER
        
        # Make Java applications aware this is a non-reparenting window manager.
        export _JAVA_AWT_WM_NONREPARENTING=1
        
        # Set default cursor.
        xsetroot -cursor_name left_ptr
        
        picom -b
        
        # Finally start Emacs
        ${pkgs.dbus.dbus-launch} --exit-with-session emacs -mm --fullscreen --internal-border=0 --border-width=0
      '';
      target = ".xinitrc";
    };
    home.packages = with pkgs; [
      boomer
      arandr
      feh
      gtk3
      i3lock-fancy
      xclip
      xorg.xev
    ];
  };
}
