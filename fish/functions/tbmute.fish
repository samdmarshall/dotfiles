function tbmute
	set arg_count (echo $argv | wc -w | awk '{print $1}')
	
	if [ $arg_count -ge 1 ];
		python ~/.config/fish/functions/python/tweetbot-mute.py $argv
	else
		echo "Please supply a keyword"
	end
end