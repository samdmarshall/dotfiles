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

set -xg GEM_HOME "$HOME/.gem"
set -u GEM_HOME_BIN "$GEM_HOME/bin"
if test ! -e $GEM_HOME_BIN
    set -e GEM_HOME_BIN
end

# setting up local search paths
set -u LOCAL_RUBY_PATH (gem environment gempath | sed -e 's=:.*$=/bin=')
if test ! -e $LOCAL_RUBY_PATH
    set -e LOCAL_RUBY_PATH
end

# setting $PATH
set fish_user_paths $LOCAL_RUBY_PATH "$XDG_CONFIG_HOME/scripts" $GEM_HOME_BIN $GOPATH_BIN "/usr/local/sbin"
