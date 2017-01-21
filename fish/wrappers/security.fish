switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function profileinfo --wraps=security
            command security cms -D -i $argv
        end
end
