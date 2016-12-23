function cal --wraps=khal
    if test (count $argv) -eq 0
        command khal interactive
    else
        command khal $argv
    end
end