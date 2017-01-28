function print_segment --argument colour --argument string_value
    printf '‌%s%s%s‌' (set_color $colour) $string_value (set_color $__fish_prompt_normal)
end

function where --description 'display the current host and working path'
    set -l where_host (print_segment $__fish_prompt_host $__fish_prompt_hostname)
    set -l where_path (print_segment $__fish_prompt_path $PWD)
    set -l where_modifier "in"
    if set -q SSH_CONNECTION
        set where_modifier "via ssh; in"
    end
    printf 'on %s %s %s\n' $where_host $where_modifier $where_path
end
