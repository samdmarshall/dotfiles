function bundleid
	set arg_count (echo $argv | wc -w | awk '{print $1}')
	if [ $arg_count -eq 1 ];
		mdfind kMDItemCFBundleIdentifier = "$argv[1]"
	else
		echo "please supply a bundle identifier"
	end
end