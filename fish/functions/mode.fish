function mode --description 'mode enabler/disabler' --argument mode_command

	set arg_count (count $argv)

	if [ $arg_count -eq 1 ];
		
		if test "$mode_command" = "android"
			if test "$ENABLED_ANDROID" = "false"
				set -xg ENABLED_ANDROID true

				set -xg ANDROID_HOME /usr/local/opt/android-sdk
				set -xg ANDROID_NDK_HOME /usr/local/opt/android-ndk
			else
				set -xg ENABLED_ANDROID false
				
				set -xg ANDROID_HOME ""
				set -xg ANDROID_NDK_HOME ""
			end

			defaults write com.pewpewthespells.fish.modes ENABLE_ANDROID $ENABLE_ANDROID
		end

		if test "$mode_command" = "work"

			set CCOLLAB_PATH ""

			if test "$ENABLED_WORK" = "false"
				set -xg ENABLED_WORK true
				sudo chmod +rx /Library/Java/JavaVirtualMachines/jdk*.jdk
				
				if test -e /Applications/ccollab_client
					set CCOLLAB_PATH /Applications/ccollab_client
				end
				
			else
				set -xg ENABLED_WORK false
				sudo chmod -rx /Library/Java/JavaVirtualMachines/jdk*.jdk
			end
			
			defaults write com.pewpewthespells.fish.modes ENABLED_WORK $ENABLED_WORK
 
			set PATH /usr/local/bin /usr/bin /bin $LOCAL_PYTHON_PATH $LOCAL_RUBY_PATH $CCOLLAB_PATH 
		end
		
	else
		echo "usage: dot [android|work]"
	end
end