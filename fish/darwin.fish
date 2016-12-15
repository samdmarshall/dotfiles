function secure_note_storage --argument name
    set keys_keychain_password (command security find-generic-password -l $KEY_STORAGE_KEYCHAIN_NAME -w)
    command security unlock-keychain -p $keys_keychain_password $KEY_STORAGE_KEYCHAIN_PATH
    set note_contents (command security find-generic-password -C note -w -l $name $KEY_STORAGE_KEYCHAIN_PATH | xxd -r -p | plutil -p - | grep "NOTE" | awk '{gsub(/"/, "", $3); print $3}')
    echo $note_contents
end

function unlock_keychain_if_necessary --argument keychain_name
    string match --ignore-case "User interaction is not allowed" (command security show-keychain-info $keychain_name 2>&1)
    if test $status -eq 0
        command security unlock-keychain $keychain_name
    end
end

unlock_keychain_if_necessary login.keychain
set -xg HOMEBREW_GITHUB_API_TOKEN (secure_note_storage HOMEBREW_GITHUB_API_TOKEN)
