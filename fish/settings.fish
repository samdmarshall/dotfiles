# disable greeting
set fish_greeting ""

set -gx HOME_CONFIG_PATH ~/.config
set -xg FISH_CONFIG_PATH $HOME_CONFIG_PATH/fish

set -x GIT_DEFAULTS_DIR $HOME_CONFIG_PATH/defaults

set -x CORE_SCRIPTS_PATH $HOME_CONFIG_PATH/scripts

set -xg LLDB_DEFAULTS_DIR $HOME_CONFIG_PATH/lldb

set -xg KEY_STORAGE_KEYCHAIN_NAME keys.keychain

set -xg KEY_STORAGE_PATH $HOME_CONFIG_PATH/storage

set -xg KEY_STORAGE_KEYCHAIN_PATH $KEY_STORAGE_PATH/$KEY_STORAGE_KEYCHAIN_NAME

set -x HOMEBREW_INSTALL_BADGE 🌈

if [ $PLATFORM_NAME = "Darwin" ];

	function unlock_login_keychain_if_necessary
		set keychain_status (security show-keychain-info login.keychain 2>&1 | grep "User interaction is not allowed" | wc -l)
		if [ $keychain_status -eq 1 ];
			security unlock-keychain login.keychain
		end
	end

	function secure_note_storage --argument name
		set keys_keychain_password (security find-generic-password -l $KEY_STORAGE_KEYCHAIN_NAME -w)
		security unlock-keychain -p $keys_keychain_password $KEY_STORAGE_KEYCHAIN_PATH
		set note_contents (security find-generic-password -C note -w -l $name $KEY_STORAGE_KEYCHAIN_PATH | xxd -r -p | plutil -p - | grep "NOTE" | awk '{gsub(/"/, "", $3); print $3}')
		echo $note_contents
	end

	if [ $HAS_SECURITY = true ];

		set found_registered_keychain (security list-keychains | grep "$KEY_STORAGE_KEYCHAIN_NAME" | wc -l)
		if [ $found_registered_keychain -eq 1 ];
		
			unlock_login_keychain_if_necessary
		
			# this will need to be updated when it changes, connect to server and netstat for bouncer connection
			set -g HOME_IP (secure_note_storage HOME_IP)

			set -x HOMEBREW_GITHUB_API_TOKEN (secure_note_storage HOMEBREW_GITHUB_API_TOKEN)
		end

	end
end