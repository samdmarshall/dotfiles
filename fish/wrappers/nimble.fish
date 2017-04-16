switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function nimble --wraps=nimble
            grimoire nimble $argv
        end
end
