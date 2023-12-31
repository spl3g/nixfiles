#!/bin/sh
while true; do
  # Check the battery level and charging status
  battery_info=$(acpi -b)

  # Extract the battery level from the output of `acpi`
  battery_level=$(echo $battery_info | grep -o "[0-9]*%" | sed "s/%//")

  # Check if the laptop is charging
  if [[ $battery_info == *"Charging"* ]]; then
    # If the laptop is charging, do nothing
    :
  else
    # If the battery level is less than 15%, send a notification
    if [ "$battery_level" -lt 15 ]; then
      notify-send "Battery Low" "Battery level is at $battery_level%. Charge your laptop."
    fi
  fi

  # Sleep for 5 minutes before checking the battery level again
  sleep 300
done
