function msmtp --wraps=msmtp
    command msmtp --file $HOME/.config/msmtp/msmtprc $argv
end

function goaccess --wraps=goaccess
    command goaccess --config-file=$HOME/.config/goaccess/goaccessrc $argv
end

function bundleid --wraps=mdfind
    command mdfind kMDItemCFBundleIdentifier = $argv
end

function show --wraps=open
    command open --reveal $argv
end

function profileinfo --wraps=security
    command security cms -D -i $argv
end

function certinfo --wraps=openssl
    command openssl x509 -inform DER -text -in $argv
end

function video2gif --wraps=ffmpeg
    command ffmpeg -vf scale=640:-1 -gifflags +transdiff ~/Desktop/out.gif -i
end

function cat --wraps=pygmentize
    if test -n (command -v pygmentize)
        command pygmentize -g $argv
    else
        command cat $argv
    end
end

function brew --wraps=brew
    command env -u GIT_CONFIG brew $argv
end
