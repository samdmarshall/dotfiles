function mktemp --wraps=mktemp
    if count $argv > /dev/null
        switch $argv[1]
            case '-s' '--suffix'
                command python -c 'import tempfile; print(tempfile.mktemp(suffix="'$argv[2]'"))'
            case '*'
                command mktemp $argv
        end
    else
        command mktemp
    end
end

complete --command mktemp --short-option s --long-option suffix --exclusive --arguments "" --description "specify a file extension for the temporary file"
