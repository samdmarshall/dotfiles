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

function svn --wraps=svn
    switch (echo $argv[1])
        case diff
            command svn diff --diff-cmd=diff $argv[2..-1]
        case commits
            command svn log -v --xml | grep '<author.*/author>' | sort | uniq -c | sort -rn | sed -e 's/<author>//g' -e 's/<\/author>//g'
        case '*'
            command svn $argv
    end
end

function fzf --wraps=fzf
    set -l file_path (mktemp)
    command fzf $argv >$file_path
    if test $status -eq 0 -a -s $file_path
        cat $file_path
    end
    if test -e $file_path
        rm $file_path
    end
end


function notmuch --wraps=notmuch
    set -l drafts_dir "$HOME/eMail-drafts"
    switch (echo $argv[1])
        case compose
            set -l temp_file (mktemp)
            set -l random_name (basename $temp_file)
            command rm $temp_file
            set -l email_draft "$drafts_dir/$random_name"
            # ask for notmuch to create a reply template and pipe it over to micro for composing a response
            command notmuch reply $argv[2..-1] >$email_draft
            command micro $email_draft
        case send
            set -l message_path $drafts_dir/$argv[2]
            if test -s $message_path
                command cat $message_path | msmtp
            end
        case sendall
            for draft in (ls $drafts_dir)
                notmuch send $draft
            end
        case apply-tags
            notmuch-apply-tags
        case '*'
            command notmuch $argv
    end
end

function otool --wraps=otool
    switch (echo $argv[1])
        case dump-plist
            command otool -s __TEXT __info_plist $argv[2] | xxd -r
        case '*'
            command otool $argv
    end
end
