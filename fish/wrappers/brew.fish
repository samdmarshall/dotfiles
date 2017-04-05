switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function brew --wraps=brew
            grimoire brew $argv
        end
end
