function make
    if test -e Makefile
        make $argv
    else
        ./make.py $argv
    end

end
