# setting up the basic environment
source $HOME/.config/fish/environment.fish

# environment variables and fish settings
source $HOME/.config/fish/settings.fish

# building prompt
source $HOME/.config/fish/prompt.fish

# common aliases
source $HOME/.config/fish/aliases.fish

# wrapper commands
for wrapper in (ls $HOME/.config/fish/wrappers)
    source $HOME/.config/fish/wrappers/$wrapper
end

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
