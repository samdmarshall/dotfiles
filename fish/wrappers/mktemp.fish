function mktemp --wraps=mktemp
    switch (echo $argv[1])
        case '--suffix'
            python -c 'import tempfile; print(tempfile.mktemp(suffix="'$argv[2]'"))'
        case '*'
            command mktemp $argv
    end
end

