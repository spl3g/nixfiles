{ pkgs, ... }:
{
  programs.kitty = {
    enable = true;
    font.name = "Source Code Pro";
    font.size = 11.3;
    theme = "Catppuccin-Mocha";
    shellIntegration.enableFishIntegration = true;
    settings = {
      cursor_shape = "underline";
      background_opacity = "0.7";
    };
  };
}
