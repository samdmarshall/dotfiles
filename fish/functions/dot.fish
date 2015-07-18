function dot --description 'dot file management' --argument dot_command

	set arg_count (echo $argv | wc -w | awk '{print $1}')

	if [ $arg_count -eq 1 ];
		set CURR (pwd)
		cd $HOME_CONFIG_PATH
		
		if test "$dot_command" = "upload"
			git add -f fish/*.fish fish/functions/*.fish defaults/* fish/functions/gensite/*
			if test -e $HOME_CONFIG_PATH/fish/functions/testfunc.fish
				git reset -- fish/functions/testfunc.fish 
			end
			git commit -m "💻"
			git push origin master
			set dot_command "reload"
		end

		if test "$dot_command" = "update"
			git pull
			set dot_command "reload"
		end

		if test "$dot_command" = "reload"
			. $FISH_CONFIG_PATH/config.fish
		end
		
		cd $CURR
	else
		echo "usage: dot [upload|update|reload]"
	end
end