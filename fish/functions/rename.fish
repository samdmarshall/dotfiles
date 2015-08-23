function rename --argument oldname --argument newname
	
	set found_items (ls -lsa | grep "$oldname" | wc -l | awk '{print $1}')
	
	if [ $found_items -gt 0 ];
		for f in *
			mv $f (echo {$f} | sed "s/$oldname/$newname/g")
		end
	end
end