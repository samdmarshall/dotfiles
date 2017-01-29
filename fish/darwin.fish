function unlock_keychain_if_necessary --argument keychain_name
    string match --ignore-case "User interaction is not allowed" (command security show-keychain-info $keychain_name 2>&1)
    if test $status -eq 1
        command security unlock-keychain $keychain_name
    end
end

if set -q SSH_CONNECTION
   unlock_keychain_if_necessary login.keychain
end

set -xg HOMEBREW_GITHUB_API_TOKEN   (secure-env --keychain:$KEY_STORAGE_KEYCHAIN_PATH --name:HOMEBREW_GITHUB_API_TOKEN)
set -xg DANGER_GITHUB_API_TOKEN     (secure-env --keychain:$KEY_STORAGE_KEYCHAIN_PATH --name:DANGER_GITHUB_API_TOKEN)
set -xg GITHUB_TOKEN                (secure-env --keychain:$KEY_STORAGE_KEYCHAIN_PATH --name:GITHUB_TOKEN)
set -xg ASCIINEMA_API_TOKEN         (secure-env --keychain:$KEY_STORAGE_KEYCHAIN_PATH --name:ASCIINEMA_API_TOKEN)
set -xg WEECHAT_PASSPHRASE          (secure-env --keychain:$KEY_STORAGE_KEYCHAIN_PATH --name:WEECHAT_PASSPHRASE)
set -xg HOMEBREW_PIPELINE_API_TOKEN (secure-env --keychain:$KEY_STORAGE_KEYCHAIN_PATH --name:HOMEBREW_PIPELINE_API_TOKEN)
set -xg GISTIT_TOKEN                (secure-env --keychain:$KEY_STORAGE_KEYCHAIN_PATH --name:GISTIT_TOKEN)

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
