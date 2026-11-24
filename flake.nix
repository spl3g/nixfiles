{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";

    home-manager.url = "github:nix-community/home-manager";
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };

    agenix = {
      url = "github:yaxitech/ragenix";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };

    agenix-rekey = {
      url = "github:oddlama/agenix-rekey";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };

    deploy-rs.url = "github:serokell/deploy-rs";
  };

  outputs = {self, nixpkgs, nixpkgs-stable, deploy-rs , agenix , agenix-rekey , disko , ...}@inputs:
    let
      globals = {
        domain = "kcu.su";
        email = "spleefer6@gmail.com";
      };

      systems = [
        "aarch64-linux"
        "i686-linux"
        "x86_64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in {

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
          {
            default = pkgs.mkShell {
              packages = with pkgs; [
                agenix-rekey.packages.${system}.default
                deploy-rs.packages.${system}.default
              ];
            }; 
          }
      );
      
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt);

      nixosModules = import ./modules;

      overlays = import ./overlays { inherit inputs; };
      
      nixosConfigurations = {
        ltrr-pad = nixpkgs-stable.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit self inputs globals; };
          modules = [
            ./hosts/ltrr-pad
            ./profiles/home
            
            agenix.nixosModules.default
            inputs.agenix-rekey.nixosModules.default

            disko.nixosModules.disko
          ];
        };
        ltrr-cloud = nixpkgs-stable.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit self inputs globals; };
          modules = [
            ./hosts/ltrr-cloud
            ./profiles/cloud

            agenix.nixosModules.default
            agenix-rekey.nixosModules.default

            disko.nixosModules.disko
          ];
        };
      };

      deploy.nodes = {
        ltrr-pad = {
          hostname = "192.168.0.101";
          profiles.system = {
            user = "root";
            path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.ltrr-pad;
            sshUser = "root";
          };
        };

        ltrr-cloud = {
          hostname = "kcu.su";
          profiles.system = {
            user = "root";
            path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.ltrr-cloud;
            sshUser = "root";
          };
        };
      };

      agenix-rekey = agenix-rekey.configure {
        userFlake = self;
        nixosConfigurations = self.nixosConfigurations;
      };
    };
}
