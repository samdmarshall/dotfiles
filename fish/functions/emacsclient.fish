# Defined in - @ line 1
function emacsclient --description 'alias emacsclient emacsclient --create-frame --no-wait'
	command emacsclient --create-frame --display $DISPLAY --no-wait $argv;
end
