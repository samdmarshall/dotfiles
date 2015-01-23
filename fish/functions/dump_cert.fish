function dump_profile

	set arg_count (echo $argv | wc -w | awk '{print $1}')
	
	if [ $arg_count -eq 1 ];
		set profile_path "$argv[1]"
		
		security cms -D -i "$profile_path"
	else
		echo "Please pass a provisioning profile path"
	end
end

function dump_cert

	set arg_count (echo $argv | wc -w | awk '{print $1}')
	
	if [ $arg_count -eq 1 ];
		set cert_path "$argv[1]"
		
		openssl x509 -in "$cert_path" -inform DER -text
	else
		echo "Please pass a certificate path"
	end
end