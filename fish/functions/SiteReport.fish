function SiteReport
	
	if test -e ~/Sites/logs
		pushd ~/Sites/logs
		
		set arg_count (count $argv)
		
		set log_file "access.log"
	
		if [ $arg_count -eq 1 ];
			set log_file "$argv[1]"
		else
			cd ~/Sites/logs
		end
	
		if which goaccess < /dev/null
			goaccess --no-global-config -f $log_file --geoip-database=/usr/local/var/GeoIP/GeoIP.dat -g --real-os -o ~/Desktop/Report.html
	
			open -a Safari ~/Desktop/Report.html
		end
		
		popd
	end
	
end