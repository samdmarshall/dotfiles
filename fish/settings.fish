# disable greeting
set fish_greeting ""

set -gx HOME_CONFIG_PATH ~/.config
set -g FISH_CONFIG_PATH $HOME_CONFIG_PATH/fish

set -x GIT_DEFAULTS_DIR $HOME_CONFIG_PATH/defaults

set -x CORE_SCRIPTS_PATH $HOME_CONFIG_PATH/scripts

set -x HOMEBREW_INSTALL_BADGE 🌈

set -x PLATFORM_NAME (uname -s)

# this will need to be updated when it changes, connect to server and netstat for bouncer connection
set -g HOME_IP 174.62.169.58
