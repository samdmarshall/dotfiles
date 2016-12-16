function env --wraps=env
    command env $argv | command sed 's/TOKEN=.*$/TOKEN=*****/g'
end
