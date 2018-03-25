
set --local platform (command uname -s)

switch (echo "$platform")
	case 'Darwin' 'darwin'
		set --export --global FISH_PLATFORM_NAME Darwin
	case 'Linux' 'linux'
		set --local windows (string match --index "*Microsoft*" (uname --all))
		if test -n $windows
			set --export --global FISH_PLATFORM_NAME Windows
		else
			set --export --global FISH_PLATFORM_NAME Linux
		end
end
