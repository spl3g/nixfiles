if ls /tmp/polhide
    polybar-msg cmd hide
    sleep 0.1
    bspc config -m focused top_padding 3
    rm /tmp/polhide
else
    polybar-msg cmd show
    touch /tmp/polhide
end
    
