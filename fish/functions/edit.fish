function edit --argument directory
    if test ! $directory = ""
        cd $directory
    end
    eval "fzf | xargs micro"
end
