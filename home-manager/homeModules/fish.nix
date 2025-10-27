{
  pkgs,
  config,
  lib,
  ...
}: {
  options = {
    fish.enable = lib.mkEnableOption "enable fish";
  };
  config = lib.mkIf config.fish.enable {
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    programs.nix-index.enableFishIntegration = true;

    programs.fish = {
      enable = true;
      plugins = [
        {
          name = "pure";
          src = pkgs.fishPlugins.pure.src;
        }
        {
          name = "autopair";
          src = pkgs.fishPlugins.autopair.src;
        }
      ];
      interactiveShellInit = ''
        set fish_greeting
        pokemon-colorscripts -r --no-title
      '';

      shellAliases = {
        ls = "ls --hyperlink=auto --color=auto";
      };
    };
  };
}
