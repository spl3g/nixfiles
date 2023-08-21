let
  more = { pkgs, ... }: {
    programs = {
      kitty = {
        enable = true;
        font.name = "Source Code Pro";
        font.size = 11.3;
        theme = "Rosé Pine Moon";
        shellIntegration.enableFishIntegration = true;
        extraConfig = "cursor_shape underline";
      };
    };
  };
in
[
  ./fish
  ./firefox
  ./rofi
  ./emacs
  ./ranger
  more
]   
