function msmtp --wraps=msmtp
    command msmtp --file $HOME/.config/msmtp/msmtprc $argv
end

function goaccess --wraps=goaccess
    command goaccess --config-file $HOME/.config/goaccess/goaccessrc $argv
end
