set -u FISH_PLATFORM_NAME (uname -s)

set -xg EDITOR micro

if test -e $HOME/.go
    set -xg GOPATH $HOME/.go
end

set -xg FZF_DEFAULT_COMMAND "pt --hidden --ignore=.git -g=''"
set -xg FZF_DEFAULT_OPTS "--preview=\"echo -e '' ; file {}; echo -e '' ; head -50 {}\""

set -xg NOTMUCH_CONFIG "$HOME/.config/notmuch/notmuch-config"

set -xg HTTPIE_CONFIG_DIR "$HOME/.config/httpie"

set -xg GIT_CONFIG "$HOME/.config/git/config"
