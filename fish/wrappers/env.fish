function senv --wraps=env
    # this is to avoid printing tokens to the command line in a human readable format
    command env | command sed 's/TOKEN=.*$/TOKEN=*****/g'
end
