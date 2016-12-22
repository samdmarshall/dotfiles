function notmuch --wraps=notmuch
    switch (echo $argv[1])
        case read
            command notmuch tag -unread -- $argv[2..-1]
        case readall
            for thread_id in (command notmuch search tag:unread | command awk '{print $1}')
                notmuch read $thread_id
            end
        case '*'
            command notmuch $argv
    end
end
