function ssh --wraps=ssh
    set additional_arguments
    if test (count $argv) -gt 1
        set additional_arguments $argv[2..-1]
    end
    switch (echo $argv[1])
        case 'pegasus'
            command ssh Pegasus.local $additional_arguments
        case 'galactica'
            command ssh Galactica.local $additional_arguments
        case 'server'
            command ssh samdm@pewpewthespells.com $additional_arguments
        case 'btmm'
            set -l back_to_my_mac_host (eval "echo show Setup:/Network/BackToMyMac | scutil | sed -n 's/.* : *\(.*\).\$/\1/p'")
            command ssh -q galactica.$back_to_my_mac_host $additional_arguments
        case '*'
            command ssh $argv
    end
end
