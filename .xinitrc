unclutter &			# hide unactive mouse
setbg &				# Set wallpaper
setxkbmap -layout us,ua -variant ,, -option 'grp:alt_shift_toggle' -option caps:escape
picom -b			# compositor

exec xmonad
