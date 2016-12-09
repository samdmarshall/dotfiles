# setting up the environment checks for what commands are supported
source ~/.config/fish/environment_config.fish

# environment variables and fish settings
source ~/.config/fish/settings.fish

# setting up paths
source ~/.config/fish/paths.fish

# building prompt
source ~/.config/fish/prompt.fish

# common aliases
source ~/.config/fish/aliases.fish

# wrapper commands
source ~/.config/fish/wrappers.fish

if test -e ~/.profile
    source ~/.profile
end

if status --is-login
    # load keybindings
    source ~/.config/fish/bindings.fish
    # configuring event handlers
    source ~/.config/fish/handlers.fish

    # print the current path and host
    path
end
