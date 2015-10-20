function dumpembedplist --argument filename

	if [ $HAS_OTOOL = true ];
		if [ $HAS_XXD = true];
		
			set arg_count (count $argv)
	
			if [ $arg_count -ge 3 ];
	
				set binname (basename "$filename")
	
				if test -e $filename
					if file $filename | grep "Mach-O" > /dev/null
						otool -X -s __TEXT __info_plist $filename | xxd -r 
					else
						echo "this is not a mach-o binary"
					end
				else
					echo "file does not exist"
				end
			else
				echo "usage: dumpembedplist <path to binary>"
			end
		end
	end 

end