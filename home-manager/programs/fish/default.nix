{ pkgs, ... }:

let
  nixPlugins = [
    { name = "colored-man-output"; src = pkgs.fishPlugins.colored-man-pages.src; }
    { name = "fzf-fish"; src = pkgs.fishPlugins.fzf-fish.src; }
    { name = "pure"; src = pkgs.fishPlugins.pure.src; }
    { name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
  ];
  customPlugins = [];
  plugins = nixPlugins ++ customPlugins;
  interactiveShellInit = ''
    set fish_greeting
    pokemon-colorscripts -r | awk "NR>1 {print}"
  '';
in
{
  programs.fish = {
    enable = true;
    inherit plugins interactiveShellInit;
  };
}
