{ inputs, ... }:
let
  imports = builtins.concatMap import [
    ./programs
    ./services
  ] ++ nonListImports;
  nonListImports = [
    inputs.hyprland.homeManagerModules.default
    inputs.ags.homeManagerModules.default
    ./hyprland
    ../general
  ];
in
{
  inherit imports;
}

