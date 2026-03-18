{
  inputs,
  self,
  withSystem,
  config,
  ...
}: {
  perSystem = {
    pkgs,
    self',
    system,
    ...
  }: let
    deployPkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [
        inputs.deploy-rs.overlays.default
        (self: super: {
          deploy-rs = {
            inherit (pkgs) deploy-rs;
            lib = super.deploy-rs.lib;
          };
        })
      ];
    };
  in {
    _module.args = {
      inherit deployPkgs;
    };
  };
  flake.deploy.nodes = {
    ltrr-block = {
      hostname = "ltrr-block";
      profiles.system = {
        user = "root";
        path = withSystem "x86_64-linux" ({deployPkgs, ...}: deployPkgs.deploy-rs.lib.activate.nixos self.nixosConfigurations.ltrr-block);
      };
      sshUser = "root";
    };
    ltrr-cloud = {
      hostname = "kcu.su";
      profiles.system = {
        user = "root";
        path = withSystem "x86_64-linux" ({deployPkgs, ...}: deployPkgs.deploy-rs.lib.activate.nixos self.nixosConfigurations.ltrr-cloud);
      };
      sshUser = "root";
    };
  };
}
