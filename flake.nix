{
  description = "NixOS configs <3";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-small.url = "github:nixos/nixpkgs/nixos-unstable-small";

    # Nix replacement because why not
    # lix-module = {
    #   url = "https://git.lix.systems/lix-project/nixos-module/archive/2.91.0.tar.gz";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    # Nix User Repo
    nurpkgs = {
      url = "github:/nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland = {
      url = "github:hyprwm/Hyprland";
    };

    # Nix command helper
    nh = {
      url = "github:viperML/nh";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    nvf.url = "github:notashelf/nvf";

    # Styling for (almost) everything
    stylix.url = "github:danth/stylix";

    # Hardware configs for my laptop
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    # niri.url = "github:/sodiboo/niri-flake";

    shyfox = {
      url = "github:Naezr/ShyFox";
      flake = false;
    };
    
    betterfox = {
      url = "github:yokoffing/Betterfox";
      flake = false;
    };
  };

  outputs = { self
            , nixpkgs
            # , lix-module
            , home-manager
            , nurpkgs
            , nixos-hardware
            , hyprland
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
            nixos-hardware.nixosModules.lenovo-thinkpad-t14-amd-gen1
          ];
        };
        ltrr = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs outputs; };
          modules = [
            ./nixos/pc/configuration.nix
          ];
        };
        ltrr-cloud = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs outputs; };
          modules = [
            ./nixos/server/configuration.nix
          ];
        };
      };
      homeConfigurations = {
        "jerpo@ltrr-mini" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs outputs; };
          modules = [
            ./home-manager/laptop.nix
            nurpkgs.modules.homeManager.default
            inputs.nvf.homeManagerModules.default
          ];
        };
        "jerpo@ltrr" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs outputs; };
          modules = [
            ./home-manager/pc.nix
            nurpkgs.modules.homeManager.default
            inputs.nvf.homeManagerModules.default
          ];
        };
      };
    };
}
