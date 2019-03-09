
# ===================================
# Functions used in environment setup
# ===================================

function __path_add --argument-names add_path
	if not contains $add_path $PATH
		if not contains $add_path $fish_user_paths
			set --append fish_user_paths $add_path
		end
	end
end

function __prefix_add --argument-names prefix
	if test \( -z $prefix \) -a \( ! -d $prefix \)
		return 0
	end

	for bin_path in $prefix/{bin,sbin}
		if test -d $bin_path
			__path_add $bin_path
		end
	end		
end
