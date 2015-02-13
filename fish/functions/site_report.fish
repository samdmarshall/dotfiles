function site_report
	
	set CURR (pwd)
	
	set arg_count (echo $argv | wc -w | awk '{print $1}')
	
	set log_file "access.log"
	
	if [ $arg_count -eq 1 ];
		set log_file "$argv[1]"
	else
		cd ~/Sites/logs
	end
	
	cat $log_file | goaccess -g -a -H -M --real-os > ~/Desktop/Report.html
	
	open -a Safari ~/Desktop/Report.html
	
	cd $CURR
end