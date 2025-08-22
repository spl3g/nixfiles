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

        telescope = {
          enable = true;
        }; 

        globals.editorconfig = true;

        keymaps = [
          {
            key = "<C-\\>";
            action = ''function() 
              vim.opt.keymap = vim.opt.keymap:get() == "russian-jcukenwin" 
                                and "" 
                                or "russian-jcukenwin"
              vim.cmd.stopinsert()
              vim.cmd.startinsert()
            end'';
            mode = ["i"];
            lua = true;
          }
          {
            key = "<C-\\>";
            action = ''<cmd>lua vim.opt.keymap = vim.opt.keymap:get() == "russian-jcukenwin" and "" or "russian-jcukenwin"<CR>'';
            mode = ["n"];
          }
        ];
      };
    };
  };
}
