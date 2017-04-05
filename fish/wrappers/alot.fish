switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function alot --wraps=alot
            grimoire alot $argv
        end
end
