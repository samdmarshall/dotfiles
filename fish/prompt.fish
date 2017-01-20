# setting up colours
set __fish_prompt_user -o d43582
set __fish_prompt_host b58900
set __fish_prompt_path 299e95
set __fish_prompt_normal normal
set __fish_prompt_hostname (hostname -s)

function battery_level --argument percentage
    set -l percentage (string trim $percentage --chars " %")
    echo $percentage
end

function fish_right_prompt
    if test -e ~/eMail/Inbox/.notmuch/
        set -l unread_emails (notmuch count tag:unread)
        if test $unread_emails -gt 0
            printf '! '
        end
    end
    set -l raw_data (string split ';' (pmset -g rawbatt))
    if test (count $raw_data) -gt 4
        set -l level (battery_level $raw_data[4])
        set -l battery_color green
        if test $level -lt 10
            set battery_color red --bold
        else if test $level -lt 25
            set battery_color red
        else if test $level -lt 50
            set battery_color yellow
        end
        printf '['
        printf '%s' (set_color $battery_color)
        printf '%s%%' $level
        printf '%s' (set_color $__fish_prompt_normal)
        printf ']'
    end
end

function fish_prompt
    if set -q SSH_CONNECTION
        printf 'Connected to -> %s\n' $__fish_prompt_hostname
    end
    printf '%s' (set_color $__fish_prompt_user)
    printf '%s' $USER
    printf '%s' (set_color $__fish_prompt_normal)
    printf ': '
end

function print_segment --argument colour --argument string_value
    printf '<‌'
    printf '%s' (set_color $colour)
    printf '%s' $string_value
    printf '%s' (set_color $__fish_prompt_normal)
    printf '‌>'
end

function where --on-variable PWD --description 'display the current host and working path'
    print_segment $__fish_prompt_host $__fish_prompt_hostname
    printf ' '
    print_segment $__fish_prompt_path (prompt_pwd)

    printf '\n'
end
