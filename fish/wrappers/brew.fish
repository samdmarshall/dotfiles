function brew --wraps=brew
    command env -u GIT_CONFIG brew $argv
end
