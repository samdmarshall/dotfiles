switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function pass --wraps=pass
            grimoire pass $argv
        end
end
