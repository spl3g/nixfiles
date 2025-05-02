#!/bin/sh

tg_workspace=$(hyprctl clients -j | jq -e '.[] | select((.class | contains("org.telegram.desktop"))) | .workspace.id')
if [[ -z $tg_workspace ]]; then
	telegram-desktop
elif [[ $tg_workspace -eq $(hyprctl activeworkspace -j | jq -e '.id') ]]; then
	hyprctl dispatch pin class:org.telegram.desktop
	hyprctl dispatch movetoworkspacesilent special:magic,class:org.telegram.desktop
else
	hyprctl dispatch movetoworkspacesilent +0,class:org.telegram.desktop
	hyprctl dispatch pin class:org.telegram.desktop
fi
