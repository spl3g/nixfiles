{ pkgs, config, lib, ... }:

{
  options = {
    greetd = {
      enable = lib.mkEnableOption "enable greetd";
      command = lib.mkOption { type = lib.types.str; };
    };
  };
  config = lib.mkIf config.greetd.enable {
    services.greetd = {
      enable = true;
      settings = {
        terminal = {
          vt = "next";
        };
        default_session = {
          command = "${pkgs.greetd.tuigreet}/bin/tuigreet -t -r --asterisks --user-menu -c ${config.greetd.command}";
          user = "jerpo";
        };
      };
    };
  };
}
