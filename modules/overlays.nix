{
  inputs,
  self,
  ...
}: {
  flake.overlays = {
    truly-unstable-packages = final: _prev: {
      unstable = import inputs.nixpkgs-small {
        system = final.system;
        config.allowUnfree = true;
      };
    };

    unstable-packages = final: _prev: {
      unstable = import inputs.nixpkgs {
        system = final.system;
        config.allowUnfree = true;
      };
    };

    stable-packages = final: _prev: {
      stable = import inputs.nixpkgs-stable {
        system = final.system;
        config.allowUnfree = true;
      };
    };

    # additions = final: _prev: import self.packages {pkgs = final;};
  };
}
