function qtfix
	set empty_string "";
	if [ "$argv" != $empty_string ];
		xattr -rd com.apple.quarantine $argv
	else
		echo "please supply a directory"
	end
end