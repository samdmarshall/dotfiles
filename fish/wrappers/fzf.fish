function fzf --wraps=fzf
    set -l file_path (command mktemp)
    command fzf $argv >$file_path
    if test $status -eq 0 -a -s $file_path
        cat $file_path
    end
    if test -e $file_path
        rm $file_path
    end
end