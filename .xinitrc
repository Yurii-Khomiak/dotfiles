unclutter &			# hide unactive mouse
setbg &				# Set wallpaper
setxkbmap -layout us,ua -variant ,, -option 'grp:alt_shift_toggle' -option caps:escape
picom -b			# compositor
dunst &
remind -z1 '-knotify-send -u critical "%s"' .config/remind/reminders.rem &

exec xmonad
