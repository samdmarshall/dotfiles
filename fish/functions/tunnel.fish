function tunnel --argument port
    switch (echo $FISH_PLATFORM_NAME)
        case 'Darwin'
            if test -z $port
                set port "1234"
            end

            set service_name (scutil --service-name)
            command sed -i -e "s=^#   ProxyCommand.*=    ProxyCommand            nc -x localhost:$port %h %p=" ~/.ssh/config
            command sudo networksetup -setsocksfirewallproxy "$service_name" localhost $port
            and ssh galactica-icloud -D $port

            trap (command sudo networksetup -setsocksfirewallproxystate "$service_name" off; and command sed -i -e 's=^    ProxyCommand=#   ProxyCommand=' ~/.ssh/config) INT

            command sudo networksetup -setsocksfirewallproxystate "$service_name" off
            command sed -i -e 's=^    ProxyCommand=#   ProxyCommand=' ~/.ssh/config
        case '*'
            echo 'tunneling through BackToMyMac is not supported on non-Darwin systems'
    end
end
