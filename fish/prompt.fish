# setting up colours
set __fish_prompt_user -o d43582
set __fish_prompt_host b58900
set __fish_prompt_path 299e95
set __fish_prompt_normal normal
set __fish_prompt_hostname (hostname -s)


function fish_right_prompt
  printf '%s %s' (coven) (batterylevel --default --color --status)
end

function fish_prompt
  printf '\n%s\n' (when)
  printf '%s\n' (where --color=always) 
  printf '(%s%s%s) ' (set_color $__fish_prompt_user) $USER (set_color $__fish_prompt_normal)
end
