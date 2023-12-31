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
    pokemon-colorscripts -r --no-title
  '';
  # loginShellInit = ''
  #   Hyprland
  # '';
  functions = {
    ranger_func = ''
      ranger $argv
      set -l quit_cd_wd_file "$HOME/.ranger_quit_cd_wd"
      if test -s "$quit_cd_wd_file"
        cd "$(cat $quit_cd_wd_file)"
        true > "$quit_cd_wd_file"
      end
    '';
  };
  shellAliases = {
    rn = "ranger_func";
    ls = "ls --hyperlink=auto --color=auto";
    iamatomic = "sudo nixos-rebuild switch --flake ~/nixfiles#ltrr";
  };
      
in
{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  
  programs.fish = {
    enable = true;
    inherit plugins interactiveShellInit  functions shellAliases;
  };
}
