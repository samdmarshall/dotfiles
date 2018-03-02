function cat --wraps=cat
    if count $argv > /dev/null
        switch $argv[1]
            case '-c' '--color'
                command pygmentize -g $argv[2..-1]
            case '*'
                command cat $argv
        end
    else
        command cat
    end
end

complete --command cat --short-option c --long-option color --description "Use `pygmentize` for syntax highlighting"
