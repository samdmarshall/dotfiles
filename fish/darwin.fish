function secure_storage --argument name
    set keys_keychain_password (command security find-generic-password -l $KEY_STORAGE_KEYCHAIN_NAME -w)
    command security unlock-keychain -p $keys_keychain_password $KEY_STORAGE_KEYCHAIN_PATH
    set note_contents (command security find-internet-password -a $name -w $KEY_STORAGE_KEYCHAIN_PATH)
    echo $note_contents
end

function unlock_keychain_if_necessary --argument keychain_name
    string match --ignore-case "User interaction is not allowed" (command security show-keychain-info $keychain_name 2>&1)
    if test ! $status -eq 0
        command security unlock-keychain $keychain_name
    end
end

if set -q SSH_CONNECTION
   unlock_keychain_if_necessary login.keychain
end

set -xg HOMEBREW_GITHUB_API_TOKEN (secure_storage HOMEBREW_GITHUB_API_TOKEN)
set -xg GITHUB_TOKEN (secure_storage GITHUB_TOKEN)

function fish_right_prompt
    if test (command defaults read com.pewpewthespells.notmuch-notifier hasmail) -ne 0
        printf '!'
    end
end

function hub --wraps=git
    # this is to allow hub to get the same autocompletes as git
    command hub $argv
end

