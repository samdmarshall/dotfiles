
# set -xg DANGER_GITHUB_HOST          (rune get --key:GITHUB_HOST)
# set -xg DANGER_GITHUB_API_HOST      (rune get --key:DANGER_GITHUB_API_HOST)
# set -xg HOMEBREW_GITHUB_API_TOKEN   (rune get --key:HOMEBREW_GITHUB_API_TOKEN)
# set -xg DANGER_GITHUB_API_TOKEN 	(rune get --key:DANGER_GITHUB_API_TOKEN)
# set -xg GITHUB_TOKEN                (rune get --key:GITHUB_TOKEN)
# set -xg ASCIINEMA_API_TOKEN         (rune get --key:ASCIINEMA_API_TOKEN)
# set -xg WEECHAT_PASSPHRASE          (rune get --key:WEECHAT_PASSPHRASE)
# set -xg GISTIT_TOKEN                (rune get --key:GISTIT_TOKEN)


function __fish_man_page --argument name --argument section
    set -l open_string ""
    set -l grep_string ""
    set -l commandline_string (command basename (commandline -po; echo)[1]) ^/dev/null
    
    if test -n $name 
        set name $commandline_string
    end

    if test -z $section
        set open_string "x-man-page:///$name"
        set grep_string "$name("
    end

    if test -z "$name"
        if test -z "$section"
            set open_string "x-man-page://$section/$name"
            set grep_string "$name($section)"
        end
    end

    if command man -k "$name" | command grep -q "$grep_string" > /dev/null
        command open "$open_string"
    else
        man $commandline_string
    end
end
