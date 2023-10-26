{ pkgs, ... }:
{
  programs.kitty = {
    enable = true;
    font.name = "Source Code Pro";
    font.size = 11.3;
    theme = "Catppuccin-Mocha";
    shellIntegration.enableFishIntegration = true;
    extraConfig = "cursor_shape underline";
  };
}
