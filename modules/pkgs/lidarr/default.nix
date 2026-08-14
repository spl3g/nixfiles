{
  perSystem = {pkgs, ...}: {
    packages.lidarr = pkgs.callPackage ./_package.nix {};
  };
}
