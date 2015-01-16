function dumpsection
	set segname "$argv[1]"
	set sectname "$argv[2]"
	set filename "$argv[3]"
	
	set binname (basename "$filename")
	
	otool -s $segname $sectname $filename > ~/Desktop/{$segname}_{$sectname}_{$binname}.data
end