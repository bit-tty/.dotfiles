#!/bin/bash
#color settings
#nvidia-settings --load-config-only
ckb-next --background
#stop screen from turning off after inactivity
xset s off
xset -dpms
xset s noblank

#dispwin -d 1 -I ~/Documents/Color\Profiles/sRGB-1.5G.icc
#dispwin -d 2 -I ~/Documents/Color\Profiles/sRGB-1.5G.icc
#
xrandr --output DP-4  --primary --mode 2560x1440 --rate 144 --gamma  0.7 --brightness 1.0
xrandr --output HDMI-0 --mode 1920x1080 --rate 60 --gamma 0.65 --brightness 1.0

#start up apps

if pidof steam >/dev/null 2>&1; then
	echo "Steam is already running"
else 
	steam&
	echo "Steam started"
fi

if pidof spotify&>/dev/null 2>&1; then
	echo "Spotify already running"
else
	/path/to/specific/spotify/spotify &
#sleep 5  # Optional: Wait a few seconds to allow Spotify to start
	env LD_PRELOAD=/usr/lib/spotify-adblock.so spotify
	echo "Spotify launched"
fi
