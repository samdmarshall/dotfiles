function xman
	
	set empty_string "";
	if [ "$argv" != $empty_string ];
		set lookup_name "$argv[-1]"
		set lookup_section "$argv[1]"
		set open_string ""
		set grep_string ""
	
		if [ $lookup_name = $lookup_section ]; set open_string "x-man-page:///$lookup_name"; set grep_string "$lookup_name("; end;
		if [ $lookup_name != $lookup_section ]; set open_string "x-man-page://$lookup_section/$lookup_name"; set grep_string "$lookup_name($lookup_section)"; end;
	
		if man -k "$lookup_name" | grep -q "$grep_string" > /dev/null
			open "$open_string"
		else
			echo "No man page found!"
		end
	else
		echo "usage: xman [name] (section)"
	end
end

