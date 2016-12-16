function env --wraps=env
    # this is to avoid printing tokens to the command line in a human readable format
    command env $argv | command sed 's/TOKEN=.*$/TOKEN=*****/g'
end
