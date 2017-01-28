# disable greeting
set fish_greeting

# know what platform this is running on
set -u FISH_PLATFORM_NAME (command uname -s)

set -u KEY_STORAGE_KEYCHAIN_NAME keys.keychain
set -u KEY_STORAGE_KEYCHAIN_PATH $HOME/.config/storage/$KEY_STORAGE_KEYCHAIN_NAME

set -xg EDITOR micro

set -xg GOPATH "$HOME/.go"

set -xg FZF_DEFAULT_COMMAND "pt --hidden --home-ptignore -g=''"
set -xg FZF_DEFAULT_OPTS "--preview=\"preview --metadata {} \""

set -xg NOTMUCH_CONFIG "$HOME/.config/notmuch/notmuch-config"

set -xg HTTPIE_CONFIG_DIR "$HOME/.config/httpie"

set -xg GIT_CONFIG "$HOME/.config/git/config"

set -xg HOMEBREW_INSTALL_BADGE 🌈
set -xg HOMEBREW_NO_INSECURE_REDIRECT true
set -xg HOMEBREW_VERBOSE true

set -xg GEM_HOME $HOME/.gem
set -u GEM_HOME_BIN $GEM_HOME/bin
if test ! -e $GEM_HOME_BIN
    set -e GEM_HOME_BIN
end

set -xg XDG_CONFIG_HOME ~/.config/

set -xg WEECHAT_HOME ~/.config/weechat/

# setting up local search paths
set -u LOCAL_RUBY_PATH (command gem environment gempath | command sed -e 's=:.*$=/bin=')
if test ! -e $LOCAL_RUBY_PATH
    set -e LOCAL_RUBY_PATH
end

# setting $PATH
set fish_user_paths $LOCAL_RUBY_PATH "$HOME/.config/scripts" $GEM_HOME_BIN
