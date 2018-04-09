function dl --argument url
	if test ! (string length $url) = 0
		command http --download "$url"
	else
		echo "Please supply url as an argument."
	end
end
