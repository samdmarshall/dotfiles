# disable greeting
set fish_greeting

# know what platform this is running on
set -u FISH_PLATFORM_NAME (command uname -s)

set -u KEY_STORAGE_KEYCHAIN_NAME keys.keychain
set -u KEY_STORAGE_KEYCHAIN_PATH $HOME/.config/storage/$KEY_STORAGE_KEYCHAIN_NAME

set -xg EDITOR micro

set -xg GOPATH "$HOME/.go"

set -xg FZF_DEFAULT_COMMAND "pt --hidden --ignore=.git -g=''"
set -xg FZF_DEFAULT_OPTS "--preview=\"echo -e '' ; file {}; echo -e '' ; cat {} \""

set -xg NOTMUCH_CONFIG "$HOME/.config/notmuch/notmuch-config"

set -xg HTTPIE_CONFIG_DIR "$HOME/.config/httpie"

set -xg GIT_CONFIG "$HOME/.config/git/config"

set -xg HOMEBREW_INSTALL_BADGE 🌈
set -xg HOMEBREW_NO_INSECURE_REDIRECT true
set -xg HOMEBREW_VERBOSE true

set -xg GEM_HOME $GEM_HOME $HOME/.gem

set -xg XDG_CONFIG_HOME ~/.config/

set -xg WEECHAT_HOME ~/.config/weechat/

set -xg MICRO_PLUGIN_MDP_DEFAULT_FLAGS "--invert --noslidenum"

# setting up local search paths
set -u LOCAL_RUBY_PATH (command gem environment gempath | command sed -e 's=:.*$=/bin=')

# setting $PATH
set fish_user_paths $LOCAL_RUBY_PATH "$HOME/.config/scripts" $GEM_HOME/bin
