function autotor
    switch (echo $FISH_PLATFORM_NAME)
        case 'Darwin'
            set port "9050"

            set service_name (scutil --service-name)
            command sed -i -e "s=^#   ProxyCommand.*=    ProxyCommand            nc -x localhost:$port %h %p=" ~/.ssh/config
            command sudo networksetup -setsocksfirewallproxy "$service_name" localhost $port
            command sudo networksetup -setsocksfirewallproxystate "$service_name" on
            and read var

            trap (command sudo networksetup -setsocksfirewallproxystate "$service_name" off; and command sed -i -e 's=^    ProxyCommand=#   ProxyCommand=' ~/.ssh/config) INT

            command sudo networksetup -setsocksfirewallproxystate "$service_name" off
            sed -i -e 's=^    ProxyCommand=#   ProxyCommand=' ~/.ssh/config
        case '*'
            echo "tor is not configured to work automatically for $FISH_PLATFORM_NAME"
    end
end
