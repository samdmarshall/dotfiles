function emacsclient-gui --description 'alias emacsclient emacsclient --create-frame --no-wait'
  command emacsclient --create-frame --eval "(eval-expression '(dotspacemacs/user-config))" --display $DISPLAY --no-wait
end
