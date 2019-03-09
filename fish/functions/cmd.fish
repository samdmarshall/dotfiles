switch $FISH_PLATFORM_NAME
	case Windows
		function cmdexe --description="starts an instance of the windows command prompt shell"
			command /mnt/c/Windows/System32/cmd.exe
		end
end
