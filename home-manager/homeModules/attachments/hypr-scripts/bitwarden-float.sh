windowtitlev2() {
  IFS=',' read -r -a args <<< "$1"
  args[0]="${args[0]#*>>}"

  if [[ ${args[1]} == "Extension: (Bitwarden Password Manager) - — Mozilla Firefox" ]]; then
    hyprctl --batch "\
      dispatch setfloating address:0x${args[0]}; \
      dispatch resizewindowpixel exact 20% 50%, address:0x${args[0]}; \
      dispatch centerwindow; \
    "
  fi
}

handle() {
  case $1 in
    windowtitlev2\>*) windowtitlev2 "$1" ;;
  esac
}

socat -U - UNIX-CONNECT:"/$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock" \
  | while read -r line; do handle "$line"; done
