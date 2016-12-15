function xmanlist  --argument lookup_name
	set open_string "x-man-page:///$lookup_name;type=a"
	command open "$open_string"
end
