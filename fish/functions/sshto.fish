function sshto
	set empty_string "";
	if [ "$argv" != $empty_string ];
		set host_connect "$argv[1]"
		
		if test "$host_connect" = "Pegasus" -o "$host_connect" = "pegasus"
			ssh sam@Pegasus.local
		else if test "$host_connect" = "Galactica" -o "$host_connect" = "galactica" 
			ssh sam@Galactica.local
		else if test "$host_connect" = "Linode" -o "$host_connect" = "linode"
			ssh samdm@samdmarshall.com
		else
			echo "invalid host!"
		end
	end
end