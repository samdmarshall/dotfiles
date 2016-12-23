function ssh --wraps=ssh
    if count $argv > /dev/null
        set -l additional_arguments ""
        if test (count $argv) -gt 1
            set additional_arguments $argv[2..-1]
        end
        switch $argv[1]
            case 'demi'
                command ssh Demi.local $additional_arguments
            case 'galactica'
                command ssh Galactica.local $additional_arguments
            case 'server'
                command ssh samdm@pewpewthespells.com $additional_arguments
            case 'btmm'
                set -l back_to_my_mac_host (eval "command echo show Setup:/Network/BackToMyMac | command scutil | command sed -n 's/.* : *\(.*\).\$/\1/p'")
                command ssh -q galactica.$back_to_my_mac_host $additional_arguments
            case '*'
                command ssh $argv
        end
    else
        command ssh
    end
end

complete --command ssh --arguments "demi galactica server btmm"
