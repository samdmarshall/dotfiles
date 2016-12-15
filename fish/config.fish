# setting up the basic environment
source $HOME/.config/fish/environment.fish

switch (echo "$FISH_PLATFORM_NAME")
    case 'Darwin'
        source $HOME/.config/fish/darwin.fish
    case 'Linux'
        source $HOME/.config/fish/linux.fish
end

# building prompt
source $HOME/.config/fish/prompt.fish

# wrapper commands
for wrapper in (ls $HOME/.config/fish/wrappers)
    source $HOME/.config/fish/wrappers/$wrapper
end

if test -e $HOME/.profile
    source $HOME/.profile
end

if status --is-login
    # load keybindings
    source $HOME/.config/fish/bindings.fish

    # configuring event handlers
    source $HOME/.config/fish/handlers.fish

    # print the current path and host
    path
end
