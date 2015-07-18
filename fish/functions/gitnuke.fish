function gitnuke
	git status | grep "deleted:" | awk '{print $2}' | xargs git rm
end