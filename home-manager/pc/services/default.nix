let
  more = { pkgs, ... }: {
    services = {
      clipmenu.enable = true;
      # polybar = {
      #   enable = true;
      #   package = pkgs.polybar.override {
      #     pulseSupport = true;
      #   };
      #   script = "polybar example &";
      # };
    };
  };
in
[
  ./picom
  more
]
