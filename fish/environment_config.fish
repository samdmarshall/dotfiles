set -u FISH_PLATFORM_NAME (uname -s)

set -xg EDITOR micro

if test -e $HOME/.go
    set -xg GOPATH $HOME/.go
end

