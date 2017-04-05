switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function asciinema --wraps=asciicema
            grimoire asciinema $argv
        end
end
