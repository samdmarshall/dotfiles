function gitcommits
	set -l current_branch (git rev-parse --abbrev-ref HEAD)
	for ref in (git rev-list --first-parent $current_branch)
		if [ (count (git branch --contains $ref)) -eq 2 ];
			set -l parent_branch (git branch --contains $ref | grep -v "$current_branch" | cut -c 3- )
			echo (git rev-list $parent_branch.. --count)
			break
		end
	end
end