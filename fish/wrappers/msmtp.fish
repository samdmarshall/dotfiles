function msmtp --wraps=msmtp
    command msmtp --file $HOME/.config/msmtp/msmtprc $argv
end
