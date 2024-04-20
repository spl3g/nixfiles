{ pkgs, config, lib, ... }:

{
  options = {
    fish.enable = lib.mkEnableOption "enable fish";
  };
  config = lib.mkIf config.fish.enable {
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    
    programs.fish = {
      enable = true;
      plugins = [
        { name = "colored-man-output"; src = pkgs.fishPlugins.colored-man-pages.src; }
        { name = "fzf-fish"; src = pkgs.fishPlugins.fzf-fish.src; }
        { name = "pure"; src = pkgs.fishPlugins.pure.src; }
        { name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
      ];
      interactiveShellInit = ''
    set fish_greeting
    pokemon-colorscripts -r --no-title
  '';
      
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
      };
    };
  };
}


