switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function pass --wraps=pass
            env -u GIT_CONFIG grimoire pass $argv
        end
end
