function notmuch --wraps=notmuch
    set -l drafts_dir "$HOME/eMail-drafts"
    switch (echo $argv[1])
        case compose
            set -l temp_file (mktemp)
            set -l random_name (basename $temp_file)
            command rm $temp_file
            set -l email_draft "$drafts_dir/$random_name"
            # ask for notmuch to create a reply template and pipe it over to micro for composing a response
            command notmuch reply $argv[2..-1] > $email_draft
            command micro $email_draft
        case send
            set -l message_path $drafts_dir/$argv[2]
            if test -s $message_path
                command cat $message_path | msmtp
            end
        case sendall
            for draft in (command ls $drafts_dir)
                notmuch send $draft
            end
        case read
            command notmuch tag -unread -- $argv[2..-1]
        case readall
            for thread_id in (command notmuch search tag:unread | command awk '{print $1}')
                notmuch read $thread_id
            end
        case apply-tags
            command notmuch-apply-tags
        case today
            command notmuch search date:today
        case yesterday
            command notmuch search date:yesterday
        case unread
            command notmuch search tag:unread
        case '*'
            command notmuch $argv
    end
end
