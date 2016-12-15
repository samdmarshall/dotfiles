# disable greeting
set fish_greeting ""

# know what platform this is running on
set -u FISH_PLATFORM_NAME (uname -s)

set -u SHELL_CONFIG_PATH $HOME/.config
set -u CORE_SCRIPTS_PATH $SHELL_CONFIG_PATH/scripts

set -u KEY_STORAGE_KEYCHAIN_NAME keys.keychain
set -u KEY_STORAGE_PATH $SHELL_CONFIG_PATH/storage
set -u KEY_STORAGE_KEYCHAIN_PATH $KEY_STORAGE_PATH/$KEY_STORAGE_KEYCHAIN_NAME

set -xg EDITOR micro

set -xg GOPATH $HOME/.go

set -xg FZF_DEFAULT_COMMAND "pt --hidden --ignore=.git -g=''"
set -xg FZF_DEFAULT_OPTS "--preview=\"echo -e '' ; file {}; echo -e '' ; head -50 {}\""

set -xg NOTMUCH_CONFIG "$HOME/.config/notmuch/notmuch-config"

set -xg HTTPIE_CONFIG_DIR "$HOME/.config/httpie"

set -xg GIT_CONFIG "$HOME/.config/git/config"

set -xg HOMEBREW_INSTALL_BADGE 🌈
set -xg HOMEBREW_NO_INSECURE_REDIRECT true
set -xg HOMEBREW_VERBOSE true

set -xg GEM_HOME $GEM_HOME $HOME/.gem

# setting up local search paths
set -u LOCAL_PYTHON_PATH (command python -m site --user-base)"/bin"
set -u LOCAL_RUBY_PATH (command gem environment gempath | sed -e 's=:.*$=/bin=')

# setting $PATH
set PATH $PATH $LOCAL_PYTHON_PATH $LOCAL_RUBY_PATH $CORE_SCRIPTS_PATH
