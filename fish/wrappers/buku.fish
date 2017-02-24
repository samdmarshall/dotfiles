function buku --wraps=buku
    env XDG_DATA_HOME="$HOME/.config" command buku $argv
end
