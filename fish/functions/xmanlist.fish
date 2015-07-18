function xmanlist
	set empty_string "";
	if [ "$argv" != $empty_string ];
		set lookup_name "$argv[1]"
		set open_string "x-man-page:///$lookup_name;type=a"
		
		open "$open_string"
	else
		echo "usage: xmanlist [name]"
	end
end