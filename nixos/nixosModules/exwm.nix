{ config, lib, ... }:

{
  options = {
    exwm.enable = lib.mkEnableOption "enable exwm";
  };

  config = lib.mkIf config.exwm.enable {
    services.xserver = {
      enable = true;
      updateDbusEnvironment = true;
      xkb = {
        layout = "us,ru";
        options = "grp:win_space_toggle";
      };
      
      displayManager.startx.enable = true;
    };
    
    services.libinput = {
        enable = true;
        touchpad = {
          naturalScrolling = true;
          disableWhileTyping = true;
        };
      };
    # Hide the cursor when typing.
    services.xbanish.enable = true;
  };
}
