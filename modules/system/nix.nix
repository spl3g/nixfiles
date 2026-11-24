{
  self,
  inputs,
  ...
}:

{
  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than=30d";
    };

    optimise.automatic = true;
    
    settings = {
      experimental-features = ["nix-command" "flakes"];
    };
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = builtins.attrValues inputs.self.overlays;
  };
}
