function notmuch --wraps=notmuch
    set -l drafts_dir "$HOME/eMail-drafts"
    set -l date_time (command date "+%s")
    switch (echo $argv[1])
        case reply
            set -l parameters (string join "-" $argv[2..-1])
            set -l new_draft "$drafts_dir/draft-$parameters.txt"
            command notmuch reply $argv[2..-1] > $new_draft
            command micro $new_draft
        case compose
            set -l parameters (string join "-" $argv[2..-1])
            set -l new_draft "$drafts_dir/draft-$parameters.txt"
            command micro $new_draft
        case send
            set -l today (command date "+%Y.%m.%d")
            set -l file_name_array (string split . (basename $argv[2]))
            set -l file_name $file_name_array[1]
            set -l extension $file_name_array[-1]
            set -l message_path "$drafts_dir/$file_name"
            if test -s $message_path
                command cat $message_path | msmtp
                if test $status -eq 0
                    command mkdir -p "~/eMail-outbox/$today"
                    command mv "$message_path" "~/eMail-outbox/$today/$file_name-$date_time.$extension"
                else
                    command echo "an error occured while sending $draft !"
                end
            end
        case sendall
            for draft in (command find $drafts_dir -name "reply-*" -or -name "mail-*")
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
