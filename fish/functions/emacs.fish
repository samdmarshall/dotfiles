function emacs --wraps=emacs
  if test (contains -- "-nw" "$argv") -o (contains -- "--no-window-system" "$argv")
    command emacs $argv
  else
	  command emacs $argv ^&1 > /dev/null &
  end
end
