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
  progConfig = { config, ... }: "${config.home.homeDirectory}/.nixfiles/home-manager/home/programs/";
  nonNixConfigs = { config, ... }: {
    xdg.configFile = {
      "ranger".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.nixfiles/home-manager/programs/ranger";
      "emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.nixfiles/home-manager/programs/emacs/init.el";
    };
  };
in
[
  ./fish
  ./firefox
  ./rofi
  more
  nonNixConfigs
]   
