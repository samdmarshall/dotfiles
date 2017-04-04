# setting up the basic environment
set -xg XDG_CONFIG_HOME "$HOME/.config"

source "$XDG_CONFIG_HOME/fish/environment.fish"

# load the platform specific configurations
switch (echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        source "$XDG_CONFIG_HOME/fish/darwin.fish"
    # case 'Linux' 'linux'
end

# building prompt
source "$XDG_CONFIG_HOME/fish/prompt.fish"

# wrapper commands
set fish_function_path $fish_function_path "$XDG_CONFIG_HOME/fish/wrappers"

# load the user's .profile file if it exists
if test -e "$HOME/.profile"
    source "$HOME/.profile"
end

# load keybindings
source "$XDG_CONFIG_HOME/fish/bindings.fish"

if status --is-login
    # configuring event handlers
    source "$XDG_CONFIG_HOME/fish/handlers.fish"
end
