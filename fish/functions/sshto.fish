function sshto
	set empty_string "";
	if [ "$argv" != $empty_string ];
		set host_connect "$argv[1]"
		
		if [ "$host_connect" = "Pegasus" ];
			ssh sam@Pegasus.local
		else if [ "$host_connect" = "Galactica" ];
			ssh sam@Galactica.local
		else if [ "$host_connect" = "Linode" ];
			ssh samdm@samdmarshall.com
		else
			echo "invalid host!"
		end
	end
end