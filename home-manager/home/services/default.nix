let
  more = {
    services = {
      clipmenu.enable = true;
      emacs = {
        enable = true;
        defaultEditor = true;
      };
    };
  };
in
[
  ./picom
  ./sxhkd
  more
]
