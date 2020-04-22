
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
  set --export --global SHELL /brew/bin/fish

  __envar_add MANPATH /usr/share/man
  __envar_add MANPATH /usr/local/share/man
  __envar_add MANPATH $HOME/.local/share/man

  ## Default Applications
  set --export --global EDITOR "emacsclient"
  set --export --global PAGER  "w3m"

  ## XDG_*
  set --export --global XDG_CACHE_HOME $HOME/.cache

  ## Application Specific
  begin
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
    switch $PLATFORM_NAME
      case '*+WSL'
        set --export --global COMPOSE_CONVERT_WINDOWS_PATHS true
        set --export --global DOCKER_HOST 'tcp://127.0.0.1:2375'

        __envar_add MANPATH /brew/share/man
        __envar_add INFOPATH /brew/share/info
    end
end


# =======================
# Path Variable Additions
# =======================

## Gem
__prefix_add $GEM_HOME

## Portable Scripts
__path_add $SCRIPTS_DIR

## Homebrew
switch $PLATFORM_NAME
  case "Darwin"
    __prefix_add /usr/local
  case "Linux*"
    __prefix_add /brew
    __path_add /mnt/c/Program\ Files/Microsoft\ VS\ Code/bin
    __path_add /mnt/c/Program\ Files/Microsoft\ VS\ Code
end

## Nimble
__prefix_add $HOME/.nimble

## ~/.local
__prefix_add $HOME/.local
