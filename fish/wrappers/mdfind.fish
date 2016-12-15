function bundleid --wraps=mdfind
    command mdfind kMDItemCFBundleIdentifier = $argv
end