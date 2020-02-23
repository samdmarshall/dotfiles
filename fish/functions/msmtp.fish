function msmtp --wraps=msmtp
  command msmtp --file $HOME/.config/msmtp/msmtprc --read-recipients --read-envelope-from $argv
end
