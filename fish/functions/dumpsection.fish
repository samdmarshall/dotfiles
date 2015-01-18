function dumpsection

	set arg_count (echo $argv | wc -w | awk '{
		print $1
	}')
	
	if [ $arg_count -ge 3 ];
		
		set segname "$argv[1]"
		set sectname "$argv[2]"
		set filename "$argv[3]"
	
		set binname (basename "$filename")
	
		if test -e $filename
			if file $filename | grep "Mach-O" > /dev/null
				otool -s $segname $sectname $filename > ~/Desktop/{$segname}_{$sectname}_{$binname}.data
			else
				echo "this is not a mach-o binary"
			end
		else
			echo "file does not exist"
		end
	else
		echo "usage: dumpsection <segment name> <section name> <path to binary>"
	end
end