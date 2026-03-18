{inputs, ...}: {
  flake.homeModules.nvim = {
    pkgs,
    config,
    lib,
    ...
  }: {
    options.customs = {
      nvim.enable = lib.mkEnableOption "enable nvim";
    };
  };
}
