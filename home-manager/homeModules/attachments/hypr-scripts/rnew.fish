#!/usr/bin/env fish

ranger $argv
set quit_cd_wd_file "$HOME/.ranger_quit_cd_wd"
if test -s "$quit_cd_wd_file"
    kitty -d "$(cat $quit_cd_wd_file)" --detach
    true >"$quit_cd_wd_file"
end
