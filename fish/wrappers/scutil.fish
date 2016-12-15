function scutil --wraps=scutil
    switch (echo $argv[1])
        case 'service-guid'
            command echo "show State:/Network/Global/IPv4" | command scutil | command grep "PrimaryService" | command awk '{print $3}'
        case 'service-name'
            set service_guid (scutil service-guid)
            command echo "show Setup:/Network/Service/$service_guid" | command scutil | command grep "UserDefinedName" | command awk -F': ' '{print $2}'
        case '*'
            command scutil $argv
    end
end
