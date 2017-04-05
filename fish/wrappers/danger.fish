switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function danger --wraps=danger
            grimoire danger $argv
        end
end
