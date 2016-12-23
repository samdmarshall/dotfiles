function xmanlist  --argument name
    set -l commandline_string (basename (commandline -po; echo)[1]) ^/dev/null
    
    if test -n $name
        set name $commandline_string
    end
    
	command open "x-man-page:///$name;type=a"
end
