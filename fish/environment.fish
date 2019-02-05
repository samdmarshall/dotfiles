# disable greeting
set fish_greeting "くコ:彡 welcome to fish"

set --export --global LC_COLLATE C

set --export --global EDITOR "emacs"
set --export --global PAGER "w3m"

set --export --global GPG_TTY (tty)

set --export --global FZF_DEFAULT_COMMAND "pt --hidden --home-ptignore -g=''"
set --export --global FZF_DEFAULT_OPTS "--preview=\"preview --metadata {} \""

set --export --global GIT_CONFIG "$XDG_CONFIG_HOME/git/config"

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
if command --search gem >/dev/null do
  set -u LOCAL_RUBY_PATH (gem environment gempath | sed -e 's=:.*$=/bin=')
  if test ! -e $LOCAL_RUBY_PATH
      set --erase LOCAL_RUBY_PATH
  end
end

set -u LINUXBREW_PATH_BIN "/home/linuxbrew/.linuxbrew/bin"
if test ! -e $LINUXBREW_PATH_BIN
  set --erase LINUXBREW_PATH_BIN
end
set -u LINUXBREW_PATH_SBIN "/home/linuxbrew/.linuxbrew/sbin"
if test ! -e $LINUXBREW_PATH_SBIN
  set --erase LINUXBREW_PATH_SBIN
end

# setting $PATH
set fish_user_paths "$HOME/.local/bin" "$HOME/.nimble/bin" $LOCAL_RUBY_PATH "$XDG_CONFIG_HOME/scripts" $LINUXBREW_PATH_BIN $LINUXBREW_PATH_SBIN $GEM_HOME_BIN $GOPATH_BIN
