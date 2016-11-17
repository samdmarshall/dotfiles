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
if not set -q __fish_prompt_user
    set -g __fish_prompt_user (set_color -o d43582)
end
if not set -q __fish_prompt_normal
    set -g __fish_prompt_normal (set_color normal)
end
if not set -q __fish_prompt_hostname
    set -g __fish_prompt_hostname (hostname -s)
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

function fish_right_prompt
    set -l display_string ""
    if test "$ENABLED_ANDROID" = "true"
        set display_string $display_string "android"
    end
    if test "$ENABLED_WORK" = "true"
        set display_string $display_string "work"
    end

    set -l display_items (string split " " "$display_string")
    for item in $display_items
        if test (string length -- "$item") -gt 0
            printf '(%s)' $item
        end
    end
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

function path --on-variable PWD --description 'display the current host and working path'
    printf '<‌'
    printf '%s' $__fish_prompt_host
    printf '%s' $__fish_prompt_hostname
    printf '%s' $__fish_prompt_normal
    printf '‌> <‌'
    printf '%s' $__fish_prompt_path
    printf '%s' (prompt_pwd)
    printf '%s' $__fish_prompt_normal
    set -l vcs (__fish_vcs_prompt)
    if test (string length $vcs)
        printf '‌> <‌'
        printf '%s' (string trim  $vcs)
    end
    printf '‌>'

    printf '\n'
end
