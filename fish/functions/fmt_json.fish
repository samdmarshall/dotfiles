function fmt_json
	set arg_count (echo $argv | wc -w | awk '{print $1}')
	
	if [ $arg_count -eq 1 ];
		
		set json_file "$argv[1]"
		
		python -m json.tool $json_file
	else
		echo "usage: fmt_json <json file>"
	end
end