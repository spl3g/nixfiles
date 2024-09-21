import subprocess

status = subprocess.run(["wpctl", "status"], stdout=subprocess.PIPE).stdout.decode('utf-8')
status_lines = status.split('\n')
sink_line = 0
selected_sink = 0
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
        selected_sink = sink
        
    sinks.append(sink)
    sink_names.append(name[:end_name].strip())

next_sink = sinks.index(selected_sink) + 1
if next_sink == len(sinks):
    next_sink = 0
    
next_sink_name = sink_names[next_sink]
subprocess.run(["wpctl", "set-default", sinks[next_sink]])

subprocess.run(["notify-send", "Pipewire sink", "Selected " + next_sink_name])

