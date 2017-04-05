switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function carthage --wraps=carthage
            grimoire carthage $argv
        end
end
