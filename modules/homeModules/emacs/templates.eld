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

python-ts-mode

(view "class " (s name) "ViewSet(viewsets.ModelViewSet):"
     n> "queryset = " (s name) ".objects.all()"
     n> "serializer_class = " (s name) "Serializer")
