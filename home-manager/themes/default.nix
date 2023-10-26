{ pkgs, ... }:

let
  themePackage = pkgs.catppuccin-gtk.override {
    accents = [ "flamingo" ];
    size = "compact";
    tweaks = [ "rimless" ];
    variant = "macchiato";
  };
  cursorPackage = pkgs.bibata-cursors;
in
{
  gtk = {
    enable = true;
    theme = {
      package = themePackage;
      name = "Catppuccin-Macchiato-Compact-Flamingo-Dark";
    };
    cursorTheme = {
      package = cursorPackage;
      name = "Bibata-Modern-Ice";
    };
    gtk4.extraCss = builtins.readFile ./Catppuccin/gtk.css;
  };
  xdg.configFile = {
    "gtk-4.0/assets" = {
      source = ./Catppuccin/assets;
      target = "gtk-4.0/assets";
    };
    "gtk-4.0/gtk-dark" = {
      source = ./Catppuccin/gtk-dark.css;
      target = "gtk-4.0/gtk-dark.css";
    };
  };
}
