{
  perSystem = {
    pkgs,
    config,
    ...
  }: {
    devShells.default = pkgs.mkShell {
      packages = with pkgs; [
        config.agenix-rekey.package
        deploy-rs
      ];
    };
  };
}
