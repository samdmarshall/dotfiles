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

if status --is-login
	# configuring event handlers
	source ~/.config/fish/handlers.fish
	current_path
end
