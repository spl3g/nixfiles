let
  workspaces = [
    "α"
    "β"
    "γ"
    "δ"
    "ε"
  ];
  monitors = {
    "^1" = workspaces;
    "^2" = workspaces;
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
in
{
  xsession.windowManager.bspwm = {
    enable = true;
    inherit monitors settings startupPrograms;
  };
}
