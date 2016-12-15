function otool --wraps=otool
    switch (echo $argv[1])
        case dump-plist
            command otool -s __TEXT __info_plist $argv[2] | xxd -r
        case '*'
            command otool $argv
    end
end