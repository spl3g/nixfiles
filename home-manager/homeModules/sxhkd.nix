{ pkgs, config, lib, ... }:

{
  options = {
    sxhkd.enable = lib.mkEnableOption "enable sxhkd";
  };
  
  config = lib.mkIf config.sxhkd.enable {
    services.sxhkd = {
      enable = true;
      keybindings = {
        "{_,shift} + Print" = "xfce4-screenshooter -{r,f}";
        "{_,shift} + control + Print" = "xfce4-screenshooter -{r,f} --save /dev/stdout | xclip -i -selection clipboard -t image/png";
        "super + apostrophe" = "betterlockscreen -l"; # Lockscreen
        "super + grave" = "polybar -r"; # Restart polybar
        "super + q" = "alacritty"; # Open terminal
        "super + d" = "rofi -show-icons -show drun"; # Open app chooser
        "super + shift + d" = "CM_LAUNCHER=rofi clipmenu";
        "super + b" = "zen-beta"; # Open browser
        "super + e" = "emacsclient -c -a 'emacs'"; # Open emacs
        "super + Escape" = "pkill -USR1 -x sxhkd"; # Restart sxhkd
        "super + shift + {e,r}" = "bspc {quit,wm -r}"; # Quit/restart bspwm
        "super + {control,shift} + q" = "bspc node -{k,c}"; # Close/kill window
        "super + m" = "bspc desktop -l next"; # Maximise window
        "super + {t,shift + t,v,f}" = "bspc node -t {tiled,pseudo_tiled,floating,fullscreen}"; # Set window state
        "super + {_,shift + }{h,j,k,l}" = "bspc node -{f,s} {west,south,north,east}"; # Focus window in the given direction
        "super + {Left,Down,Up,Right}" = "bspc node -v {-20 0,0 20,0 -20,20 0}"; # Move a floating window
        "super + s : {h,j,k,l}" = ''STEP=20; SELECTION={1,2,3,4};\
  bspc node -z $(echo "left -$STEP 0,bottom 0 $STEP,top 0 -$STEP,right $STEP 0" | cut -d',' -f$SELECTION) ||\
  bspc node -z $(echo "right -$STEP 0,top 0 $STEP,bottom 0 -$STEP,left $STEP 0" | cut -d',' -f$SELECTION)''; # Better window resize
        "super + bracket{left,right}" = "bspc desktop -f {prev,next}.local"; # Focus next/previos desktop
        "super + {_,shift + }{1-9,0}" = "bspc {desktop -f,node -d} $(bspc query -D -m focused | awk 'NR=={1-9,0}')"; # Focus/send window to the given desktop on the focused monitor
        "super + o" = "bspc node -m last -f"; # Send window to the last used monitor
        "super + ctrl + {1-9}" = "bspc node -o 0.{1-9}"; # Preselect the window ratio
        "super + ctrl + space" = "bspc node -p cancel"; # Cansel the preselected ratio
        "super + n" = "fish ~/.nixfiles/home-manager/home/services/polybar/hide.fish";
      };
      };
  };
}
