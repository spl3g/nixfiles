#!/bin/sh

declare -a cmd

usage() {
    echo -e "-m | monitor\n-s | slurp\n-w | active window\n-c | add copy"
}

monitor() {
    cmd=("grim -o \"\$(hyprctl -j monitors | jq -r '.[] | select(.focused) | .name')\"")
}

slurp() {
    cmd=("grim -g \"\$(slurp)\"")
}

window() {
    cmd=("grim -g \"\$(hyprctl activewindow -j | jq -j '\"\(.at | .[0]),\(.at | .[1]) \(.size | .[0])x\(.size | .[1])\"')\"")
}

copy() {
    if [[ -n ${cmd[0]} ]]; then
        cmd+=("- | wl-copy")
    else
        usage
    fi
}

while getopts ":mswc" opt; do
    case ${opt} in
        m) monitor;;
        s) slurp;;
        w) window;;
        c) copy;;
        *) usage
           exit 1;;
    esac
done

if [[ -z $1 ]]; then
    usage
    exit 1
fi

bash -c "${cmd[*]}"
