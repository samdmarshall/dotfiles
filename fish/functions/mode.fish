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
		end

		if test "$mode_command" = "work"
			if test "$ENABLED_WORK" = "false"
				set -xg ENABLED_WORK true
				sudo chmod +rx /Library/Java/JavaVirtualMachines/jdk*.jdk
			else
				set -xg ENABLED_WORK false
				sudo chmod -rx /Library/Java/JavaVirtualMachines/jdk*.jdk
			end
		end
		
		dot reload
		
	else
		echo "usage: dot [android|work]"
	end
end