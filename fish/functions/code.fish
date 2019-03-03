if test $FISH_PLATFORM_NAME = "Windows"
	set --unexport vs_code_path "/mnt/c/Program Files/Microsoft VS Code/Code.exe"
	function code --wraps=$vs_code_path
		command $vs_code_path $argv ^&1 > /dev/null &
	end
end
