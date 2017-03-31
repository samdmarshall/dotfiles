switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function http --wraps=http
            grimoire http $argv
        end
end
