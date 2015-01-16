function shortcuts
	set _COMMAND_KEY_SYMBOL "@"
	set _CONTROL_KEY_SYMBOL "^"
	set _OPTION_KEY_SYMBOL "~"
	set _TAB_KEY_SYMBOL "\\U21e5"
	
    # Show Package Contents: Command-Shift-O
    defaults write com.apple.finder NSUserKeyEquivalents "{ 'Show Package Contents' = '{$_COMMAND_KEY_SYMBOL}{$_OPTION_KEY_SYMBOL}{$_CONTROL_KEY_SYMBOL}O'; }"
    if not defaults read com.apple.universalaccess "com.apple.custommenu.apps" | grep "com.apple.finder" > /dev/null
        # does not contain app
        defaults write com.apple.universalaccess "com.apple.custommenu.apps" -array-add "com.apple.finder"
    else
        # contains app already, so do nothing
    end	
end