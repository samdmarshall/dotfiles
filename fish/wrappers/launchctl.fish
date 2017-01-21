switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function launchctl --wraps=launchctl
            if test (count $argv) -ge 1
                switch $argv[1]
                    case 'list-system'
                        command launchctl list | command head -n1
                        command launchctl list | command grep "com.apple"
                    case 'list-other'
                        command launchctl list | command grep -v "com.apple"
                    case '*'
                        command launchctl $argv
                end
            else
                command launchctl
            end
        end
        complete --command launchctl -n '__fish_use_subcommand' -xa 'list-system\t"'(_ "list only services not provided by the system")'"'
        complete --command launchctl -n '__fish_use_subcommand' -xa 'list-other\t"'(_ "list only services not provided by the system")'"'
end
