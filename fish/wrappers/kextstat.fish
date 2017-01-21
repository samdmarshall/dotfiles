switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function kextstat --wraps=kextstat
            if test (count $argv) -ge 1
                switch $argv[1]
                    case '--system'
                        command kextstat | head -1 
                        command kextstat | command grep "com.apple"
                    case '--other'
                        command kextstat | command grep -v "com.apple"
                    case '*'
                        command kextstat $argv
                end
            else
                command kextstat
            end
        end
        complete --command kextstat --long-option "system" --description "display system kernel extensions"
        complete --command kextstat --long-option "other" --description "display third-party kernel extensions"
end
