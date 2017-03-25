switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function pass --wraps=pass
            command env -u GIT_CONFIG pass $argv
        end
end
