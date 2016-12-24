# setting up colours
set __fish_prompt_user -o d43582
set __fish_prompt_host b58900
set __fish_prompt_path 299e95
set __fish_prompt_normal normal
set __fish_prompt_hostname (hostname -s)

function fish_right_prompt
    ~/.config/scripts/mystatus.py status
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
