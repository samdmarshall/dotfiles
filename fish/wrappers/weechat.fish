switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function weechat --wraps=weechat
            grimoire weechat $argv
        end
end
