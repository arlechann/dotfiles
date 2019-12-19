#!/bin/bash

color='#439ad3'
circle=('◜  ' '  ◝' '  ◞' '◟  ')
[[ $(cat /sys/class/power_supply/AC/online) = "1" ]] && { \
	echo -n "${circle[$(($(date +%S) % 4))]} "
	color='#ff7f50'
}

battery=$([[ -e /sys/class/power_supply/BAT0 ]] && cat /sys/class/power_supply/BAT0/capacity)
battery_status=$([[ -e /sys/class/power_supply/BAT0 ]] && cat /sys/class/power_supply/BAT0/status)

[[ -n $battery ]] && { \
	if [[ $battery -ge 75 ]]; then
		color='#08d137'
	elif [[ $battery -le 20 ]]; then
		color='#f73525'
	fi
	echo "$battery% "
} || echo

echo -e "\n$color"
