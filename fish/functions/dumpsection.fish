function dumpsection --argument segname --argument sectname --argument filename

	if [ $HAS_OTOOL = true]; 
		set arg_count (count $argv)
	
		if [ $arg_count -ge 3 ];
	
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
	else
		echo "requires otool installed!"
	end
end