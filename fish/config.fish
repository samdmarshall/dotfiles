set -x HOME_CONFIG_PATH ~/.config
set -x FISH_CONFIG_PATH $HOME_CONFIG_PATH/fish

# environment variables and fish settings
source $FISH_CONFIG_PATH/settings.fish
# setting up paths
source $FISH_CONFIG_PATH/paths.fish
# building prompt
source $FISH_CONFIG_PATH/prompt.fish
# configuring event handlers
source $FISH_CONFIG_PATH/handlers.fish


alias reload ". $FISH_CONFIG_PATH/config.fish"