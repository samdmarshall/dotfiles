# disable greeting
set fish_greeting ""

set -gx HOME_CONFIG_PATH ~/.config
set -g FISH_CONFIG_PATH $HOME_CONFIG_PATH/fish

set -x GIT_DEFAULTS_DIR $HOME_CONFIG_PATH/defaults

set -x CORE_SCRIPTS_PATH $HOME_CONFIG_PATH/scripts

set -xg KEY_STORAGE_PATH $HOME_CONFIG_PATH/storage/keys.keychain

set -x HOMEBREW_INSTALL_BADGE 🌈

set -x PLATFORM_NAME (uname -s)

function secure_note_storage --argument name
	set keys_keychain_password (security find-generic-password -l keys.keychain -w)
	security unlock-keychain -p $keys_keychain_password $KEY_STORAGE_PATH
	set note_contents (security find-generic-password -C note -w -l $name $KEY_STORAGE_PATH | xxd -r -p | plutil -p - | grep "NOTE" | awk '{gsub(/"/, "", $3); print $3}')
	echo $note_contents
end

if which security > /dev/null

	# this will need to be updated when it changes, connect to server and netstat for bouncer connection
	set -g HOME_IP (secure_note_storage HOME_IP)

	set -x HOMEBREW_GITHUB_API_TOKEN (secure_note_storage HOMEBREW_GITHUB_API_TOKEN)

end