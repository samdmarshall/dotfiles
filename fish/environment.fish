# disable greeting
set fish_greeting "くコ:彡 welcome to fish"

# know what platform this is running on
set -u FISH_PLATFORM_NAME (command uname -s)

set -xg EDITOR "micro"
set -xg PAGER "w3m"

set -xg GOPATH "$HOME/.go"
set -u GOPATH_BIN "$GOPATH/bin"
if test ! -e $GOPATH_BIN
    set -e GOPATH_BIN
end

set -xg FZF_DEFAULT_COMMAND "pt --hidden --home-ptignore -g=''"
set -xg FZF_DEFAULT_OPTS "--preview=\"preview --metadata {} \""

set -xg GIT_CONFIG "$XDG_CONFIG_HOME/git/config"

set -xg WEECHAT_HOME "$XDG_CONFIG_HOME/weechat/"

set -xg NOTMUCH_CONFIG "$XDG_CONFIG_HOME/notmuch/notmuch-config"

set -xg GEM_HOME "$HOME/.gem"
set -u GEM_HOME_BIN "$GEM_HOME/bin"
if test ! -e $GEM_HOME_BIN
    set -e GEM_HOME_BIN
end

# setting up local search paths
set -u LOCAL_RUBY_PATH (command gem environment gempath | command sed -e 's=:.*$=/bin=')
if test ! -e $LOCAL_RUBY_PATH
    set -e LOCAL_RUBY_PATH
end

# setting $PATH
set fish_user_paths $LOCAL_RUBY_PATH "$XDG_CONFIG_HOME/scripts" $GEM_HOME_BIN $GOPATH_BIN

if command -s rune > /dev/null
    set -xg GITHUB_TOKEN                (rune get --key:GITHUB_TOKEN)
    set -xg ASCIINEMA_API_TOKEN         (rune get --key:ASCIINEMA_API_TOKEN)
    set -xg WEECHAT_PASSPHRASE          (rune get --key:WEECHAT_PASSPHRASE)
    set -xg HOMEBREW_PIPELINE_API_TOKEN (rune get --key:HOMEBREW_PIPELINE_API_TOKEN)
    set -xg OMNI_SYNC_MAILDROP_ADDR     (rune get --key:OMNI_SYNC_MAILDROP_ADDR)
end
