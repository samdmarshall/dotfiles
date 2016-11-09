function autotor
	if [ $FISH_PLATFORM_NAME = "Darwin" ];
		set port "9050"

		set service_guid (printf "open\nget State:/Network/Global/IPv4\nd.show" | scutil | grep "PrimaryService" | awk '{print $3}')
		set service_name (printf "open\nget Setup:/Network/Service/$service_guid\nd.show" | scutil | grep "UserDefinedName" | awk -F': ' '{print $2}')
		sed -i -e "s=^#   ProxyCommand.*=    ProxyCommand            nc -x localhost:$port %h %p=" ~/.ssh/config
		sudo networksetup -setsocksfirewallproxy "$service_name" localhost $port
		sudo networksetup -setsocksfirewallproxystate "$service_name" on; and read var
	
		trap (sudo networksetup -setsocksfirewallproxystate "$service_name" off; and sed -i -e 's=^    ProxyCommand=#   ProxyCommand=' ~/.ssh/config) INT
	
		sudo networksetup -setsocksfirewallproxystate "$service_name" off
		sed -i -e 's=^    ProxyCommand=#   ProxyCommand=' ~/.ssh/config
	end
end