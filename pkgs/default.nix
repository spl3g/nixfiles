{ pkgs ? import <nixpkgs> { } }: {
  tgs2png = pkgs.callPackage ./tgs2png { };
  pokemon-colorscripts = pkgs.callPackage ./pokemon-colorscripts { };
  tdlib = pkgs.callPackage ./tdlib { };
  spmp = pkgs.callPackage ./spmp { };
  spmp-server = pkgs.callPackage ./spmp-server { };
}
