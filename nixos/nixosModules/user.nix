{ pkgs, config, lib, ... }:

{
  options = {
    user.enable = lib.mkEnableOption "default user configuration";
  };
  config = lib.mkIf config.user.enable {
    programs.fish.enable = true;
    users.users = {
      jerpo = {
        isNormalUser = true;
        shell = pkgs.fish;
        extraGroups = [ "networkmanager" "wheel" "docker" "libvirtd" "input" "adbusers"  "dialout" "uinput" ];
      };
    };

  };
}
