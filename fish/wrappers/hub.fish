switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        complete --command hub --wraps=git
end
