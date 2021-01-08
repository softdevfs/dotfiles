setxkbmap -layout es
xcompgr -c -l0 -t0 -r0 -o.00 &
# dunst &
~/Scripts/loop_wp.py &
xrdb ~/.Xresources
# xrandr --auto
# ~/Scripts/warning_hour.py &
sh /home/sergio/Scripts/Bash/set_monitors.sh
exec qtile
