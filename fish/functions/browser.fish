function browser
	for url in $argv
		cmdexe /c start microsoft-edge:"$url"
	end
end
