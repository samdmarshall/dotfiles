switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function pwgen --wraps=pwgen
            command pwgen -s 32 1 $argv | tr -d '\n' | tr -d ' ' | pbcopy
        end
end
