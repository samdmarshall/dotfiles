
# =====================================
# Configuration Variables (Not Exposed)
# =====================================

set --unexport --local GIT_CONFIG_DIR $XDG_CONFIG_HOME/git
set --unexport --local SCRIPTS_DIR    $XDG_CONFIG_HOME/scripts

# =================================
# Configuration Variables (Exposed)
# =================================

begin
	## General
	set fish_greeting "くコ:彡 welcome to fish"
	set fish_emoji_width 2

	## System
	set --export --global LANG "en_US.utf8"

	## Default Applications
	set --export --global EDITOR "emacs"
	set --export --global PAGER  "w3m"
	set --export --global TERM   "xterm"

	## Application Specific
	begin
		### GPG
		set --export --global GPG_TTY (tty)

		### fzf
		set --export --global FZF_DEFAULT_COMMAND "pt --hidden --home-ptignore -g=''"
		set --export --global FZF_DEFAULT_OPTS    '--preview="preview --metadata {} "'

		### git
		set --export --global GIT_CONFIG $GIT_CONFIG_DIR/config

		## NVM
		set --export --global NVM_DIR    $HOME/.nvm

		## Gem
		set --export --global GEM_HOME   $HOME/.gem
	end
end


# ==========================
# Platform Specific Settings
# ==========================

switch $PLATFORM_NAME
	case 'Darwin'
	case 'Linux*'
		set --export --global DISPLAY      127.0.0.1:0
		set --export --global XCURSOR_SIZE 48
		switch $PLATFORM_NAME
			case '*+WSL'
				set --export --global COMPOSE_CONVERT_WINDOWS_PATHS true
				set --export --global DOCKER_HOST 'tcp://127.0.0.1:2375'
				set --export --global PULSE_SERVER 'tcp://127.0.0.1'
		end
end


# =======================
# Path Variable Additions
# =======================

## Go
__prefix_add ~/.go

## Gem
__prefix_add $GEM_HOME

## Portable Scripts
__path_add $SCRIPTS_DIR

## Homebrew
switch $PLATFORM_NAME
	case "Darwin"
		__prefix_add /usr/local
	case "Linux*"
		__prefix_add /home/linuxbrew/.linuxbrew
end

## Nimble
__prefix_add ~/.nimble

## ~/.local
__prefix_add ~/.local

