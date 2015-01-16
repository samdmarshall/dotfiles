# alias to xcrunner (https://github.com/samdmarshall/xcrunner) or xcrun
function xc
	if which xcrunner > /dev/null
		xcrunner $argv
	else
		xcrun $argv
	end
end