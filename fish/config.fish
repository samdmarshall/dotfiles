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
set VENDOR_COMPLETIONS ""
if test -e /usr/local/share/fish/vendor_completions.d
	set VENDOR_COMPLETIONS /usr/local/share/fish/vendor_completions.d
end
set fish_function_path $fish_function_path "$XDG_CONFIG_HOME/fish/wrappers" "$VENDOR_COMPLETIONS"

# load the user's .profile file if it exists
if test -e "$HOME/.profile"
	  source "$HOME/.profile"
end

# load keybindings
source "$XDG_CONFIG_HOME/fish/bindings.fish"

if status --is-login
	string match --quiet "*Microsoft*" (uname --all)
	if test $status != 0
    # configuring event handlers
    source "$XDG_CONFIG_HOME/fish/handlers.fish"
	end
end

if test "$PWD" != "$HOME"
	if test "$START_UP" != "1"
		cd $HOME
		set -xg START_UP 1
	end
end

