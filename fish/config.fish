
set --export --global XDG_CONFIG_HOME $HOME/.config

set --unexport --local FISH_CONFIG_DIR $XDG_CONFIG_HOME/fish

# ============================
# Fish User Configuration File
# ============================

source $FISH_CONFIG_DIR/platform.fish
source $FISH_CONFIG_DIR/{$PLATFORM_NAME}.fish || true
source $FISH_CONFIG_DIR/environment-functions.fish
source $FISH_CONFIG_DIR/environment.fish
source $FISH_CONFIG_DIR/prompt.fish
source $FISH_CONFIG_DIR/bindings.fish
source $FISH_CONFIG_DIR/wrappers.fish || true

## Only load when attached to something, unused otherwise
if status is-interactive
  source $FISH_CONFIG_DIR/handlers.fish
  source $FISH_CONFIG_DIR/process-agent.fish

  setup-process-agent "emacs --daemon"

  if test (pgrep -fxc "gpg-agent") -eq "0"
    eval "gpg-agent"
  end

  if test (pgrep -fxc "ssh-agent") -eq "0"
    eval "ssh-agent"
  end

	## Kitty (Terminal) setup
	if not functions --query __kitty_completions
		source (kitty + complete setup fish | psub)
	end

	## pyenv setup
	if not functions --query pyenv
		source (pyenv init -| psub)
	end
end

