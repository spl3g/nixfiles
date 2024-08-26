{ pkgs ? import <nixpkgs> { } }: {
  tgs2png = pkgs.callPackage ./tgs2png { };
  pokemon-colorscripts = pkgs.callPackage ./pokemon-colorscripts { };
  tdlib = pkgs.callPackage ./tdlib { };
  spmp = pkgs.callPackage ./spmp { };
  spmp-server = pkgs.callPackage ./spmp-server { };
  hydralauncher = pkgs.callPackage ./hydralauncher { };
  hydraclient = pkgs.callPackage ./hydralauncher/client.nix { };
  spoofdpi = pkgs.callPackage ./spoofdpi { };
}
