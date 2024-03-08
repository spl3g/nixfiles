{ inputs, ... }:
let
  imports = builtins.concatMap import [
    ./programs
    ./services
  ] ++ [
    ./hyprland
    ../general
    inputs.hyprland.homeManagerModules.default
  ];
in
{
  inherit imports;
}

