# setting up the basic environment
source "$HOME/.config/fish/environment.fish"

# load the platform specific configurations
switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        source "$HOME/.config/fish/darwin.fish"
    case 'Linux' 'linux'
        source "$HOME/.config/fish/linux.fish"
end

# building prompt
source "$HOME/.config/fish/prompt.fish"

# wrapper commands
set fish_function_path $fish_function_path "$HOME/.config/fish/wrappers"

# load the user's .profile file if it exists
if test -e "$HOME/.profile"
    source "$HOME/.profile"
end

# load keybindings
source "$HOME/.config/fish/bindings.fish"

if status --is-login
    # configuring event handlers
    source "$HOME/.config/fish/handlers.fish"
end
