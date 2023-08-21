let
  more = { pkgs, ... }: {
    services = {
      clipmenu.enable = true;
      emacs = {
        enable = true;
        defaultEditor = true;
      };
      polybar = {
        enable = true;
        package = pkgs.polybar.override {
          pulseSupport = true;
        };
        script = "polybar example &";
      };
    };
  };
in
[
  ./picom
  ./sxhkd
  ./dunst
  more
]
