function svn --wraps=svn
    if count $argv > /dev/null
        switch $argv[1]
            case diff
                command svn diff --diff-cmd=diff $argv[2..-1]
            case commits
                command svn log -v --xml | grep '<author.*/author>' | sort | uniq -c | sort -rn | sed -e 's/<author>//g' -e 's/<\/author>//g'
            case '*'
                command svn $argv
        end
    else
        command svn
    end
end
