function msmtp --wraps=msmtp
    msmtp --file=~/.config/msmtp/msmtprc $argv
end
