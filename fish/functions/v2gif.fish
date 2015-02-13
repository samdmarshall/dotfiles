function v2gif

	set arg_count (echo $argv | wc -w | awk '{print $1}')
	
	if [ $arg_count -eq 1 ];
		set video_path "$argv[1]"
		
		ffmpeg -i $video_path -vf super2xsai,scale=w=iw/2:h=ih/2 ~/Desktop/out.gif
	else
		echo "Please pass a video file path"
	end
end