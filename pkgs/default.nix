{ pkgs ? import <nixpkgs> { } }: {
  tgs2png = pkgs.callPackage ./tgs2png { };
  pokemon-colorscripts = pkgs.callPackage ./pokemon-colorscripts { };
  tdlib = pkgs.callPackage ./tdlib { };
  amnezia-client = pkgs.qt6Packages.callPackage ./amnezia-client { };
}
