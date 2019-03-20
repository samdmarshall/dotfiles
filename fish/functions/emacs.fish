
function emacs --wraps=emacs
    set --local nox_short_flag (if contains -- -nw $argv; echo 1 ; else; echo 0; end)
    set --local nox_long_flag (if contains -- --no-window-system $argv; echo 1; else; echo 0; end)

    if test \( $nox_short_flag -eq 1 \) -o \( $nox_long_flag -eq 1 \)
	command emacs $argv
    else
	command emacs $argv 2>&1 > /dev/null &
    end
end

alias cmacs="emacs --no-window-system"

#function emacs --wraps=emacs
#  command emacs $argv ^&1 > /dev/null &
#end

#function emacs-nox --wraps=emacs
#  emacs --no-window-system $argv
#end
