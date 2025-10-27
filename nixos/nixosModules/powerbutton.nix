{ pkgs, config, lib, ... }:

{
  options = {
    pbutton.disable = lib.mkEnableOption "enable pbutton";
  };
  config = lib.mkIf config.pbutton.disable {
    services.logind.settings.Login = {
      HandlePowerKey = "ignore";
      HandleLidSwitch = "suspend";
      HandleLidSwitchExternalPower = "suspend";
    };
  };
}
