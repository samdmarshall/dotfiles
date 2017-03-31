switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function brew --wraps=brew
            env -u GIT_CONFIG grimoire brew $argv
        end
end
