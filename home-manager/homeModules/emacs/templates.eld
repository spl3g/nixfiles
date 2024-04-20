nix-mode

(opt "{ pkgs, config, lib, ... }:"
     n
     n "{"
     n> "options = {"
     n> (p "option name" name) ".enable = lib.mkEnableOption \"enable " (s name) "\";"
     n "  };"
     n> "config = lib.mkIf config." (s name) ".enable {"
     n> q
     n "  };"
     n "}")
