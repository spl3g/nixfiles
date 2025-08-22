{ pkgs, lib, config, ... }:

{
  options = {
    tmux.enable = lib.mkEnableOption "enable tmux config";
  };

  config = lib.mkIf config.tmux.enable {
    stylix.targets.tmux.enable = true;
    home.packages = with pkgs; [
      fzf
    ];
    programs.tmux = {
      enable = true;
      prefix = "C-x";
      baseIndex = 1;
      historyLimit = 10000;
      extraConfig = ''
        set -g mode-keys vi 
        set -g default-terminal "''${TERM}"
        set -sg terminal-overrides ",*:RGB"

        set -g pane-border-lines simple

        set -g escape-time 0
        set -g renumber-windows on

        set -g status-style bg=default,fg=black,bright
        set -g status-left ""
        set -g window-status-format " #W "
        set -g window-status-current-format " #W "

        set -g window-status-bell-style "bg=red,nobold"
        set -g window-status-current-style \
            "#{?window_zoomed_flag,bg=yellow,bg=green,nobold}"

        bind j next-window
        bind k previous-window
      '';
      plugins = with pkgs.tmuxPlugins; [
        tmux-fzf
      ];
    };
  };
}
