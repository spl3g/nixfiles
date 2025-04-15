#!/usr/bin/env python3

import subprocess

status = subprocess.run(["wpctl", "status"], stdout=subprocess.PIPE).stdout.decode('utf-8')
status_lines = status.split('\n')
sink_line = 0
sinks = []
sink_names = []

# Get sink line
for i in range(len(status_lines) - 1):
    if 'Sinks:' in status_lines[i]:
        sink_line = i + 1
        break

# Find unused sinks
for line in status_lines[sink_line:]:
    if line == ' │  ':
        break
    line = line.split('.')
    nums = line[0]
    name = line[1]
    end_name = line[1].index('[')
    sink = nums[8:]
    if nums[4] == '*':
        continue
        
    sinks.append(sink)
    sink_names.append(name[:end_name].strip())

if (len(sinks) == 0):
    subprocess.run(["notify-send", "Pipewire sink", "No sinks to switch to"])
    exit(0)

selected_sink = subprocess.run(["rofi", "-dmenu"], input="\n".join(sink_names), capture_output=True, text=True).stdout[:-1]

if len(selected_sink) == 0:
    exit(0)

try:
    next_sink = sink_names.index(selected_sink)
except Exception:
    subprocess.run(["notify-send", "Pipewire sink", "Sink not found"])
    exit(1)
    
subprocess.run(["wpctl", "set-default", sinks[next_sink]])

subprocess.run(["notify-send", "Pipewire sink", "Selected " + selected_sink])
