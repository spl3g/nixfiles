{ pkgs, config, lib, ... }:

{
  options = {
    nvim.enable = lib.mkEnableOption "enable nvim";
  };
}

