function ssh --wraps=ssh
    switch (echo $argv[1])
        case 'pegasus'
            command ssh Pegasus.local $argv[2..-1]
        case 'galactica'
            command ssh Galactica.local $argv[2..-1]
        case 'server'
            command ssh samdm@pewpewthespells.com $argv[2..-1]
        case 'btmm'
            set -l back_to_my_mac_host (eval "echo show Setup:/Network/BackToMyMac | scutil | sed -n 's/.* : *\(.*\).\$/\1/p'")
            command ssh galatica.$back_to_my_mac_host $argv[2..-1]
        case '*'
            command ssh $argv
    end
end