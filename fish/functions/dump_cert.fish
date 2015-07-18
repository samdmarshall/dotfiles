function dump_cert

	set arg_count argv_c
	
	if [ $arg_count -eq 1 ];
		set cert_path "$argv[1]"
		
		openssl x509 -in "$cert_path" -inform DER -text
	else
		echo "Please pass a certificate path"
	end
end