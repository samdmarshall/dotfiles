# disable greeting
set fish_greeting "please run 'security unlock-keychain' to unlock the keychain!" # this is necessary so that email works!

# know what platform this is running on
set -u FISH_PLATFORM_NAME (command uname -s)

set -xg EDITOR emacs
set -xg PAGER w3m

set -xg GOPATH "$HOME/Projects/go-lang"

set -xg FZF_DEFAULT_COMMAND "pt --hidden --home-ptignore -g=''"
set -xg FZF_DEFAULT_OPTS "--preview=\"preview --metadata {} \""

set -xg GIT_CONFIG "$HOME/.config/git/config"

set -xg GEM_HOME $HOME/.gem
set -u GEM_HOME_BIN $GEM_HOME/bin
if test ! -e $GEM_HOME_BIN
    set -e GEM_HOME_BIN
end

set -xg XDG_CONFIG_HOME ~/.config/

# setting up local search paths
set -u LOCAL_RUBY_PATH (gem environment gempath | sed -e 's=:.*$=/bin=')
if test ! -e $LOCAL_RUBY_PATH
    set -e LOCAL_RUBY_PATH
end

set -u LOCAL_CARGO_PATH $HOME/.cargo/bin
if test ! -e $LOCAL_CARGO_PATH
  set -e LOCAL_CARGO_PATH
end

# setting $PATH
set fish_user_paths $LOCAL_RUBY_PATH "$HOME/.config/scripts" $GEM_HOME_BIN $LOCAL_CARGO_PATH


set -xg DANGER_GITHUB_HOST "github.bamtechmedia.co"
set -xg DANGER_GITHUB_API_BASE_URL "https://github.bamtechmedia.co/api/v3"

set -xg ANDROID_HOME "/usr/local/share/android-sdk"
set -xg ANDROID_NDK_HOME "/usr/local/share/android-ndk"
