
set --unexport --local rune_path     (command --search rune)
set --unexport --local grimoire_path (command --search grimoire)

if test -z $rune_path
	echo "cannot find `rune` in PATH!"
	exit 1
end

if test -z $grimoire_path
	echo "cannot find `grimoire` in PATH!"
	exit 1
end

if test \( ! -x $rune_path \) -o \( ! -x  $grimoire_path \)
	echo "grimoire and rune not found!! exiting early!!"
	exit 1
end

for app in (grimoire --list-enabled)
	if test -n (command --search $app)
		alias $app "grimoire $app"
	end
end

