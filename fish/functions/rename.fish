function rename --argument oldname --argument newname	
	for file in (command ls -a | command grep "$oldname")
	    command mv $file (command echo {$file} | command sed "s=$oldname=$newname=g")
	end
end
