if [ $FISH_PLATFORM_NAME = "Darwin" ]

    function unlock_login_keychain_if_necessary
        set keychain_status (security show-keychain-info login.keychain 2>&1 | grep "User interaction is not allowed" | wc -l)
        if [ $keychain_status -eq 1 ]
            security unlock-keychain login.keychain
        end
    end

    function secure_note_storage --argument name
        set keys_keychain_password (security find-generic-password -l $KEY_STORAGE_KEYCHAIN_NAME -w)
        security unlock-keychain -p $keys_keychain_password $KEY_STORAGE_KEYCHAIN_PATH
        set note_contents (security find-generic-password -C note -w -l $name $KEY_STORAGE_KEYCHAIN_PATH | xxd -r -p | plutil -p - | grep "NOTE" | awk '{gsub(/"/, "", $3); print $3}')
        echo $note_contents
    end

    set found_registered_keychain (security list-keychains | grep "$KEY_STORAGE_KEYCHAIN_NAME" | wc -l)
    if [ $found_registered_keychain -eq 1 ]
        unlock_login_keychain_if_necessary
        set -x HOMEBREW_GITHUB_API_TOKEN (secure_note_storage HOMEBREW_GITHUB_API_TOKEN)
    end
end

