let
  anchor = "bottom-right";
  backgroundColor = "#1f1d2e";
  borderColor = "#e0def4";
  textColor = "#e0def4";
  defaultTimeout = 5000;
  borderSize = 3;
  borderRadius = 7;
  font = "Rubik 11";
in
{
  services.mako = {
    enable = true;
    inherit anchor
      backgroundColor
      borderColor
      textColor
      defaultTimeout
      borderRadius
      font
      borderSize;
  };
}
