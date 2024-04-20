{ pkgs, config, lib, ... }:

{
  options = {
    docker.enable = lib.mkEnableOption "enable docker";
  };
  config = lib.mkIf config.docker.enable {
    virtualisation.docker = {
      enable = true;
      liveRestore = true;
      daemon.settings = {
        bip = "172.20.0.1/16";
        default-address-pools = [{
          base = "172.20.0.0/8";
          size = 16;
        }];
      };
    };
  };
}
