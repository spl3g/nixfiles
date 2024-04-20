{
  description = "NixOS configs <3";

  inputs = {
    # Nixpkgs
    nixpkgs.url = github:nixos/nixpkgs/nixos-unstable;
    nixpkgs-small.url = github:nixos/nixpkgs/nixos-unstable-small;

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

    nh = {
      url = github:viperML/nh;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixvim = {
      url = "github:nix-community/nixvim";
      # If you are not running an unstable channel of nixpkgs, select the corresponding branch of nixvim.
      # url = "github:nix-community/nixvim/nixos-23.05";

      inputs.nixpkgs.follows = "nixpkgs";
    };
    niri.url = github:/sodiboo/niri-flake;

    stylix.url = github:danth/stylix;
  };

  outputs = { self
            , nixpkgs
            , nixpkgs-small
            , home-manager
            , nurpkgs
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
          specialArgs = { inherit inputs outputs; };
          modules = [
            ./nixos/laptop/configuration.nix
            nurpkgs.nixosModules.nur
          ];
        };
        ltrr = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs outputs; };
          modules = [
            ./nixos/pc/configuration.nix
            nurpkgs.nixosModules.nur
          ];
        };
      };
      homeConfigurations = {
        "jerpo@ltrr-mini" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs outputs; };
          modules = [
            ./home-manager/laptop.nix
          ];
        };
        "jerpo@ltrr" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs outputs; };
          modules = [
            ./home-manager/pc.nix
          ];
        };
      };
    };
}
