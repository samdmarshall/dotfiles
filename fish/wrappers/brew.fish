switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function brew --wraps=brew
            command env -u GIT_CONFIG brew $argv
        end
end
