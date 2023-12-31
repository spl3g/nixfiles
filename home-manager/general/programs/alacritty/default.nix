{ pkgs, ... }:

{
  home.packages = [ pkgs.alacritty-theme ];
  programs.alacritty = {
    enable = true;
    settings = {
      import = [ "${pkgs.alacritty-theme}/catppuccin_macchiato.yaml" ];
      font = {
        normal = {
          family = "Source Code Pro";
          style = "Medium";
        };
        size = 11.5;
      };
      window.opacity = 0.7;
      cursor = {
        style = "Beam";
        thickness = 0.27;
      };
    };
  };
}
