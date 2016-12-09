# fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate '↯'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'

# setting up colours
if status --is-login
    if not set -q __fish_prompt_user
        set -g __fish_prompt_user (set_color -o d43582)
    end
    if not set -q __fish_prompt_host
        set -g __fish_prompt_host (set_color b58900)
    end
    if not set -q __fish_prompt_path
        set -g __fish_prompt_path (set_color 299e95)
    end
    if not set -q __fish_prompt_normal
        set -g __fish_prompt_normal (set_color normal)
    end
end
if not set -q __fish_prompt_hostname
    set -g __fish_prompt_hostname (hostname -s)
end

function fish_prompt
    if set -q SSH_CONNECTION
        printf 'Connected to -> %s\n' $__fish_prompt_hostname
    end
    printf '%s' $__fish_prompt_user
    printf '%s' $USER
    printf '%s' $__fish_prompt_normal
    printf ': '
end

function print_segment --argument colour --argument string_value
    printf '<‌'
    printf '%s' $colour
    printf '%s' $string_value
    printf '%s' $__fish_prompt_normal
    printf '‌>'
end

function path --on-variable PWD --description 'display the current host and working path'
    if status --is-login
        print_segment $__fish_prompt_host $__fish_prompt_hostname
        printf ' '
        print_segment $__fish_prompt_path (prompt_pwd)

        printf '\n'
    end
end
