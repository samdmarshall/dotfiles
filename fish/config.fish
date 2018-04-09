# setting up the basic environment
set --export --global XDG_CONFIG_HOME "$HOME/.config"

source "$XDG_CONFIG_HOME/fish/environment.fish"
source "$XDG_CONFIG_HOME/fish/platform.fish"

# load the platform specific configurations
switch (echo "$FISH_PLATFORM_NAME")
  case 'Darwin'
    source "$XDG_CONFIG_HOME/fish/darwin.fish"
  case 'Windows'
    source "$XDG_CONFIG_HOME/fish/windows.fish"
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
	if test $FISH_PLATFORM_NAME != "Windows"
    # configuring event handlers
    source "$XDG_CONFIG_HOME/fish/handlers.fish"
	end
end

# this is to ensure we start in the linux home dir rather than the windows home dir when working in WSL
if test "$PWD" != "$HOME"
	if test "$START_UP" != "1"
		cd $HOME
		set --export --global START_UP 1
	end
end
