set -u FISH_PLATFORM_NAME (uname -s)

set -xg EDITOR micro

if test -e $HOME/.go
    set -xg GOPATH $HOME/.go
end

set -xg FZF_DEFAULT_COMMAND "pt --hidden --ignore=.git -g=''"
set -xg FZF_DEFAULT_OPTS "--preview=\"echo -e '' ; file {}; echo -e '' ; head -50 {}\""
