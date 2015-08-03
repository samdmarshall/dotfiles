function testfunc
	set arg_count (echo $argv | wc -w | awk '{print $1}')
	
end