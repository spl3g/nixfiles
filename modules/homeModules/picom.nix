{inputs, ...}: {
  flake.homeModules.picom = {
    pkgs,
    config,
    lib,
    ...
  }: {
    options.customs = {
      picom.enable = lib.mkEnableOption "enable picom";
    };

    config = lib.mkIf config.customs.picom.enable {
      services.picom = {
        enable = true;
        settings = {
          fading = true;
          fade-delta = 3;
          blur-background = false;
          corner-radius = 8;
          # blur = {
          #   method = "dual_kawase";
          #   size = 12;
          #   deviation = false;
          #   strength = 2;
          #   kern = "3x3box";
          # };
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
    };
  };
}
