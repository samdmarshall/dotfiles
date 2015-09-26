function SiteReport
	
	if test -e ~/Sites/logs
		set arg_count (count $argv)
	
		set log_file "access.log"
	
		if [ $arg_count -eq 1 ];
			set log_file "$argv[1]"
		else
			cd ~/Sites/logs
		end
	
		if which goaccess < /dev/null
			cat $log_file | goaccess -g -a -H -M --real-os > ~/Desktop/Report.html
	
			open -a Safari ~/Desktop/Report.html
		end
	end
	
end