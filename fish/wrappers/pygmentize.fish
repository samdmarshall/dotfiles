function cat --wraps=pygmentize
    if test -n (command -v pygmentize)
        command pygmentize -g $argv
    else
        command cat $argv
    end
end