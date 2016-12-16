function hub --wraps=git
    # this is to allow hub to get the same autocompletes as git
    command hub $argv
end
