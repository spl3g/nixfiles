#!/bin/sh
TG_CLASS=org.telegram.desktop

tg_workspace=$(hyprctl clients -j | jq -e ".[] | select((.class | contains(\"${TG_CLASS}\"))) | .workspace.id")
if [[ -z $tg_workspace ]]; then
	telegram-desktop
elif [[ $tg_workspace -eq $(hyprctl activeworkspace -j | jq -e '.id') ]]; then
	hyprctl dispatch pin class:$TG_CLASS
	hyprctl dispatch movetoworkspacesilent special:magic,class:$TG_CLASS
else
	hyprctl dispatch movetoworkspacesilent +0,class:$TG_CLASS
	hyprctl dispatch pin class:$TG_CLASS
	hyprctl dispatch focuswindow
fi
