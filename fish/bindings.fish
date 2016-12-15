function invoke_xman
    xman (commandline)
end

function invoke_xmanlist
    xmanlist (commandline)
end

function fish_user_key_bindings
    bind \cc 'commandline ""'

    switch (echo $FISH_PLATFORM_NAME)
    case 'Darwin'
        bind -k f1 invoke_xman
        bind -k f2 invoke_xmanlist
    end
end

