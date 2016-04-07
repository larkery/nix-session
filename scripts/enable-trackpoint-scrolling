#!/bin/bash
 
# extract device id
s=`xinput | grep TrackPoint`
s="${s#*id=}"
id="${s%%[!0-9]*}"
 
xinput --set-prop --type=int --format=8 "$id" "Evdev Wheel Emulation" 1
xinput --set-prop --type=int --format=8 "$id" "Evdev Wheel Emulation Button" 2
xinput --set-prop --type=int --format=8 "$id" "Evdev Wheel Emulation Axes" 6 7 4 5
