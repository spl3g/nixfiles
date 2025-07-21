{ pkgs, lib, config, ... }:

{
  options = {
    nvf.enable = lib.mkEnableOption "enable bspwm";
  };

  config = lib.mkIf config.nvf.enable {
    stylix.targets.nvf.enable = true;
    programs.nvf = {
      enable = true;
      settings.vim = {
        lsp = {
          enable = true;
        };

        autocomplete.blink-cmp = {
          enable = true;
          mappings = {
            next = "M-j";
            previous = "M-k";
            confirm = "M-i";
          };
          setupOpts = {
            completion = {
              menu.auto_show = false;
              ghost_text = {
                enabled = true;
                show_with_menu = false;
              };
            };
          };
        };

        languages = {
          enableTreesitter = true;
          enableFormat = true;

          nix.enable = true;

          ts.enable = true;
          go.enable = true;
          zig.enable = true;
        };
        
        binds.whichKey.enable = true;

        globals.editorconfig = true;
      };

      keymaps = [
        {
          key = "C-\\";
          action = ''vim.opt.keymap = vim.opt.keymap == "russian-juckenwin" and "" or "russian-juckenwin"'';
          lua = true;
        }
      ];
    };
  };
}
