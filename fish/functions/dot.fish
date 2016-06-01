function dot --description 'dot file management' --argument dot_command

	set arg_count (count $argv)

	if [ $arg_count -eq 1 ];
		pushd $HOME_CONFIG_PATH
		
		if test "$dot_command" = "upload"
			gitnuke
			git add -f $FISH_CONFIG_PATH/*.fish $FISH_CONFIG_PATH/functions/* $GIT_DEFAULTS_DIR/* $CORE_SCRIPTS_PATH/* $LLDB_DEFAULTS_DIR/* $KEY_STORAGE_PATH/*
			git commit -m "💻"
			git push
			set dot_command "reload"
		end

		if test "$dot_command" = "update"
			git pull
			set dot_command "reload"
		end
		
		if test "$dot_command" = "status"
			git status | grep ":   "
		end

		if test "$dot_command" = "reload"
			. $FISH_CONFIG_PATH/config.fish
		end
		
		popd
	else
		echo "usage: dot [upload|update|status|reload]"
	end
end