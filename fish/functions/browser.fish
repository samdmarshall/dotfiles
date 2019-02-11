function browser
	for url in $argv
		command cmd.exe /c start microsoft-edge:"$url"
	end
end
