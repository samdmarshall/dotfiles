set --unexport --local platform

switch (command uname --kernel-name)
	case "Darwin" "darwin"
		set --append platform "Darwin"
	case "Linux" "linux"
		set --append platform "Linux"
end

switch (command uname --kernel-release)
	case "*-Microsoft"
		set --append platform "WSL"
end

set --export --global PLATFORM_NAME (string join '+' $platform)
set --export --global PLATFORM_ARCH (command uname --processor)
set --export --global PLATFORM_OS   (command uname --operating-system)

