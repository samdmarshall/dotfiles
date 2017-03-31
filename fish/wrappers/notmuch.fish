switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function notmuch --wraps=notmuch
            grimoire notmuch $argv
        end
end
