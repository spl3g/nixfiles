{
  description = "NixOS configs <3";

  inputs = {
    # Nixpkgs
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;

    # Home manager
    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    nurpkgs = {
      url = github:/nix-community/NUR;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland = {
      url = github:hyprwm/Hyprland;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-colors.url = github:Misterio77/nix-colors;
  };

  outputs = { self
            , nixpkgs
            , home-manager
            , nurpkgs
            , hyprland
            , nix-colors
            , ... }@inputs:
    let
      inherit (self) outputs;
      forAllSystems = nixpkgs.lib.genAttrs [
        "aarch64-linux"
        "i686-linux"
        "x86_64-linux"
      ];
    in
    {
      packages = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in import ./pkgs { inherit pkgs; }
      );
      # Devshell for bootstrapping
      # Acessible through 'nix develop' or 'nix-shell' (legacy)
      devShells = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in import ./shell.nix { inherit pkgs; }
      );

      # Your custom packages and modifications, exported as overlays
      overlays = import ./overlays { inherit inputs outputs; };
      # Reusable nixos modules you might want to export
      # These are usually stuff you would upstream into nixpkgs
      nixosModules = import ./modules/nixos;
      # Reusable home-manager modules you might want to export
      # These are usually stuff you would upstream into home-manager
      homeManagerModules = import ./modules/home-manager;

      # NixOS configuration entrypoint
      # Available through 'nixos-rebuild --flake .#your-hostname'
      nixosConfigurations = {
        ltrr-mini = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs outputs nix-colors; };
          modules = [
            ./nixos/laptop/configuration.nix
            nurpkgs.nixosModules.nur
          ];
        };
        ltrr = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs outputs nix-colors; };
          modules = [
            ./nixos/pc/configuration.nix
            nurpkgs.nixosModules.nur
          ];
        };
      };
    };
}
