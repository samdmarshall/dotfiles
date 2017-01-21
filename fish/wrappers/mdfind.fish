switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function bundleid --wraps=mdfind
            command mdfind kMDItemCFBundleIdentifier = $argv
        end
end
