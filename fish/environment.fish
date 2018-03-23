# disable greeting
set fish_greeting "くコ:彡 welcome to fish"

# know what platform this is running on
set --export --global FISH_PLATFORM_NAME (command uname -s)

set --erase --unexport LS_COLORS
set --export --global LC_COLLATE C

set --export --global EDITOR "emacs"
set --export --global PAGER "w3m"

set --export --global GPG_TTY (tty)

set --export --global FZF_DEFAULT_COMMAND "pt --hidden --home-ptignore -g=''"
set --export --global FZF_DEFAULT_OPTS "--preview=\"preview --metadata {} \""

set --export --global GIT_CONFIG "$XDG_CONFIG_HOME/git/config"

set --export --global DOCKER_HOST "tcp://172.20.247.219:2376"
set --export --global DOCKER_CERT_PATH "/mnt/c/Users/Demi/.docker/machine/machines/default/"
set --export --global COMPOSE_CONVERT_WINDOWS_PATHS true

set --export --global GEM_HOME "$HOME/.gem"
set --local GEM_HOME_BIN "$GEM_HOME/bin"
if test ! -e $GEM_HOME_BIN
    set --erase GEM_HOME_BIN
end

set --export --global NVM_DIR "$HOME/.nvm"
if test ! -e $NVM_DIR
 	set --erase NVM_DIR
end

# setting up local search paths
set -u LOCAL_RUBY_PATH (gem environment gempath | sed -e 's=:.*$=/bin=')
if test ! -e $LOCAL_RUBY_PATH
    set --erase LOCAL_RUBY_PATH
end

# setting $PATH
set fish_user_paths "$HOME/.local/bin" "$HOME/.nimble/bin" $LOCAL_RUBY_PATH "$XDG_CONFIG_HOME/scripts" $GEM_HOME_BIN $GOPATH_BIN
