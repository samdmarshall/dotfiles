if test $FISH_PLATFORM_NAME = "Windows"
	set --unexport wsudo_path "/mnt/c/ProgramData/chocolatey/bin/wsudo.exe"
	set --unexport powershell_path "/mnt/c/Windows/SysWOW64/powershell.exe"
	function powershell --wraps=$powershell_path
		command $wsudo_path $powershell_path $argv  &
	end
end
