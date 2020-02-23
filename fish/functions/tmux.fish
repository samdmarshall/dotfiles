function tmux --wraps=tmux
  command tmux -f ~/.config/tmux/config $argv
end
