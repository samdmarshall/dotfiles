switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function gistit --wraps=gistit
            grimoire gistit -priv \"$argv\"
        end
end
