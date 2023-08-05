{ config, pkgs, lib, ... }:

{
  home.username = "jerpo";
  home.homeDirectory = "/home/jerpo";
  home.stateVersion = "23.05";
  home.packages = with pkgs; [
    bat
    emacs
    nerdfonts
    pokemon-colorscripts-mac
    kitty
    telegram-desktop
    feh
    rofi
    htop
    ranger
    betterlockscreen
    clipboard-jh
    polybar
    xdragon
    obsidian
    xclip
    lutris
    steam
    mangohud
    gamemode
    vkdt
    transmission-gtk
    rubik
    obsidian
    osu-lazer
    spotify
    figma-linux
    easyeffects
    darktable
    python311Packages.python-lsp-server
    python311Packages.pyls-flake8
    python311Packages.python-lsp-black
  ];
  programs = {
    home-manager.enable = true;
    firefox = {
      enable = true;
      profiles.Betterfox = {
        isDefault = true;
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          bitwarden
          ghostery
          switchyomega
          sponsorblock
          return-youtube-dislikes
        ];
        extraConfig = builtins.readFile ./home/programs/firefox/user.js;
        search = {
          engines = {
            "Brave" = {
              urls = [{ template = "https://search.brave.com/search?q={searchTerms}"; }];
              iconUpdateURL = "https://cdn.search.brave.com/serp/v2/_app/immutable/assets/safari-pinned-tab.539899c7.svg";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!br" ];
            };
            "NixOS" = {
              urls = [{ template = "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query={searchTerms}"; }];
              iconUpdateURL = "https://nixos.org/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!ns" ];
            };
            "HomeManager" = {
              urls = [{ template = "https://mipmip.github.io/home-manager-option-search/?query={searchTerms}"; }];
              iconUpdateURL = "https://github.com/mipmip/home-manager-option-search/blob/main/images/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!hs" ];
            };
            "ai question" = {
              urls = [{ template = "https://iask.ai/?mode=question&q={searchTerms}"; }];
              iconUpdateURL = "https://iask.ai/favicons/favicon-32x32-650bd8771fdea8866630408578e381cc.png?vsn=d";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!aq" ];
            };
            "ai forums" = {
              urls = [{ template = "https://iask.ai/?mode=forums&q={searchTerms}"; }];
              iconUpdateURL = "https://iask.ai/favicons/favicon-32x32-650bd8771fdea8866630408578e381cc.png?vsn=d";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!af" ];
            };
            "ai wiki" = {
              urls = [{ template = "https://iask.ai/?mode=wiki&q={searchTerms}"; }];
              iconUpdateURL = "https://iask.ai/favicons/favicon-32x32-650bd8771fdea8866630408578e381cc.png?vsn=d";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!aw" ];
            };
            "FastGPT" = {
              urls = [{ template = "https://labs.kagi.com/fastgpt?query={searchTerms}"; }];
              definedAliases = [ "!fq" ];
            };
            "NixWiki" = {
              urls = [{ template = "https://nixos.wiki/index.php?search={searchTerms}&go=Go"; }];
              iconUpdateURL = "https://nixos.org/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000;
              definedAliases = [ "!nw" ];
            };
          };
          default = "Brave";
        };
      };
    };
    fish = {
      enable = true;
      interactiveShellInit = ''
        set fish_greeting
        pokemon-colorscripts -r | awk "NR>1 {print}"
        set -gx EDITOR "emacsclient -c -a \"emacs\""
        '';
      plugins = [
        { name = "colored-man-output"; src = pkgs.fishPlugins.colored-man-pages.src; }
        { name = "fzf-fish"; src = pkgs.fishPlugins.fzf-fish.src; }
        { name = "pure"; src = pkgs.fishPlugins.pure.src; }
        { name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
      ];
    };
    kitty = {
      enable = true;
      font.name = "Source Code Pro";
      font.size = 11.3;
      theme = "Rosé Pine Moon";
      shellIntegration.enableFishIntegration = true;
      extraConfig = "cursor_shape underline";
    };
  };
  xsession.windowManager.bspwm = {
    enable = true;
    monitors = {
      "^1" = [
        "α"
        "β"
        "γ"
        "δ"
        "ε"
      ];
      "^2" = [
        "α"
        "β"
        "γ"
        "δ"
        "ε"
      ];
    };
    settings = {
      focused_border_color = "#908caa";
      normal_border_color = "#363a4f";
      presel_feedback_color = "#752f20";
      border_width = 3;
      window_gap = 12;
      focus_follows_pointer = true;
      split_ratio = 0.5;
    };
    startupPrograms = [
      "picom -b"
      "setxkbmap -option grp:alt_shift_toggle,grp:win_space_toggle us,ru"
      "emacs --daemon"
      "feh --bg-fill ~/dotfiles/cat.png"
    ];
  };
  services = {
    sxhkd = {
      enable = true;
      keybindings = {
        # Apps
        "{_,shift} + {_,control} + Print" = "xfce4-screenshooter -{r,f} {_,-c}"; # Screenshooter
        "super + apostrophe" = "betterlockscreen -l"; # Lockscreen
        "super + grave" = "polybar -r"; # Restart polybar
        "super + q" = "kitty"; # Open terminal
        "super + d" = "rofi -show-icons -show drun"; # Open app chooser
        "super + shift + d" = "CM_LAUNCHER=rofi clipmenu";
        "super + b" = "firefox"; # Open browser
        "super + e" = "emacsclient -c -a 'emacs'"; # Open emacs
        "super + shift + o" = "obsidian"; # Open obsidian
        # Bspwm
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
      };
    };
    picom = {
      enable = true;
      settings = {
        fading = true;
        fade-in-step = 0.05;
        fade-out-step = 0.05;
        blur-background = true;
        corner-radius = 8;
        blur = {
          method = "dual_kawase";
          size = 12;
          deviation = false;
          strength = 2;
          kern = "3x3box";
        };
        backend = "glx";
        vsync = true;
        mark = {
          wmwin-focused = true;
          overdir-focused = true;
        };
        detect = {
          rounded-corners = true;
          client-opacity = true;
          transient = true;
        };
        use-ewmh-active-win = true;
        glx-no-stencil = true;
        use-damage = true;
      };
      wintypes = {
        tooltip = {
          fade = true;
          shadow = true;
          full-shadow = false;
          blur = false;
          focus = true;
        };
        dock = {
          shadow = false;
        };
      };
    };
    clipmenu.enable = true;
  };
  gtk = {
    cursorTheme = {
      name = "Bibata-Modern-Ice";
      package = pkgs.bibata-cursors;
    };
    iconTheme = {
      name = "rose-pine-moon";
      package = pkgs.rose-pine-icon-theme;
    };
    theme = {
      name = "rose-pine-moon";
      package = pkgs.rose-pine-gtk-theme;
    };
  };
}
