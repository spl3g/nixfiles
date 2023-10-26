let
  more  = { pkgs, inputs, ... }: {
    home.packages = with pkgs; [
      kile-wl
    ];
  };
in
[
  # ./bspwm
  ./hyprland
  more
]
