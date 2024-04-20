{ pkgs, config, lib, helpers, inputs, ... }:

{
  imports = [
    inputs.nixvim.homeManagerModules.nixvim
  ];

  options = {
    nvim.enable = lib.mkEnableOption "enable nvim";
  };
  
  config = lib.mkIf config.nvim.enable {
    programs.nixvim = {
      opts = {
        number = true;
        relativenumber = true;
        tabstop = 4;
        softtabstop = 4;
        smartindent = true;
        expandtab = true;
        scrolloff = 5;
      };
      colorschemes.catppuccin.enable = true;
      globals.mapleader = " ";

      keymaps = [
        # Most used
        {
          action = ":Telescope file_browser<CR>";
          key = "<leader>.";
        }
        {
          action = ":Telescope buffers sort_lastused=true<CR>";
          key = "<leader>,";
        }
        # File related
        {
          action = ":Telescope frecency<CR>";
          key = "<leader>fr";
        }
        {
          action = ":Telescope find_files<CR>";
          key = "<leader>ff";
        }
        {
          action = ":Telescope find_files cwd=~/nixfiles<CR>";
          key = "<leader>fn";
        }
        {
          action = ":Telescope projects<CR>";
          key = "<leader>op";
        }
        # Terminal
        {
          action = ":ToggleTerm direction=vertical<CR>";
          key = "<leader>ot";
        }
        {
          action = ":ToggleTerm direction=horizontal<CR>";
          key = "<leader>oT";
        }
        {
          action = "<C-\\><C-N>";
          key = "<esc>";
          mode = "t";
        }
      ];
      autoCmd = [
        {
          event = "FileType";
          pattern = [ "sql" "mysql" "plsql" ];
          command = "lua require('cmp').setup.buffer({ sources = {{ name = 'vim-dadbod-completion' }} })";
        }
      ];

      # lsps
      plugins = {
        lsp = {
          enable = true;
          servers = {
            pyright.enable = true;
            nixd.enable = true;
          };
        };

        cmp = {
          enable = true;
          autoEnableSources = true;
          settings = {
            snippet.expand = "luasnip";
            sources = [
              {name = "nvim_lsp";}
              {name = "luasnip";}
              {name = "path";}
              {name = "buffer";}
            ];
            mapping = {
              "<CR>" = "cmp.mapping.confirm({select = false})";
              "<M-j>" = "cmp.mapping.select_next_item(cmp_select_opts)";
              "<M-k>" = "cmp.mapping.select_prev_item(cmp_select_opts)";
              "<Tab>" = "cmp.mapping.confirm({select = true})";
            };
          };
        };

        luasnip.enable = true;
      };

      plugins = {
        nix.enable = true;
        comment-nvim.enable = true;
        nvim-autopairs.enable = true;
        intellitab.enable = true;
        project-nvim.enable = true;
        undotree.enable = true;
        treesitter = {
          enable = true;
          indent = true;
          nixvimInjections = true;
        };
        codeium-vim = {
          enable = true;
          settings = {
            disable_binds = true;
            manual = true;
            no_map_tab = true;
          };
          keymaps.accept = "<M-Tab>";
        };
        toggleterm = {
          enable = true;
          autochdir = true;
          persistSize = false;
          size = "function(term)
                if term.direction == 'horizontal' then
                  return 15
                elseif term.direction == 'vertical' then
                  return vim.o.columns * 0.4
                end
              end";
        };
        telescope = {
          enable = true;
          defaults =  {
            mappings = {
              i = {
                "<M-j>" = "move_selection_next";
                "<M-k>" = "move_selection_previous";
                "<M-m>" = "toggle_selection";
                "<Tab>" = "select_default";
              };
              n = {
                "<M-m>" = "toggle_selection";
                "<M-j>" = "move_selection_next";
                "<M-k>" = "move_selection_previous";
                "<Tab>" = "select_default";
              };
            };
          };
          extensions = {
            file-browser = { 
              enable = true;
              path = "%:p:h";
            };
            frecency.enable = true;
            project-nvim.enable = true;
          };
        };
        lualine = { 
          enable = true;
          sectionSeparators = {
            left = " ";
            right = " ";
          };
          componentSeparators = {
            left = " ";
            right = " ";
          };
        };
      };
      extraPlugins = with pkgs; with vimPlugins; [
        vim-dadbod-ui
        vim-dadbod-completion
        direnv-vim
      ];
      extraPackages = with pkgs; [
        sqls
      ];
      extraConfigLua = builtins.readFile ./extralua.lua;
    };
  };
}

