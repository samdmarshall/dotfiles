function emacs --wraps=emacs
	command emacs $argv ^&1 > /dev/null &
end
