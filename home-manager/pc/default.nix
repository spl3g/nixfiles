let
  imports = builtins.concatMap import [
    ./services
    ./bspwm
  ] ++ [
    ../general
  ];
in
{
  inherit imports;
}

