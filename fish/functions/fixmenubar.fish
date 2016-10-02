function fixmenubar
	if [ $FISH_PLATFORM_NAME = "Darwin" ];
		sudo chmod 600 /System/Library/CoreServices/Search.bundle/Contents/MacOS/Search
		sudo chmod 600 /System/Library/CoreServices/Spotlight.app/Contents/MacOS/Spotlight
		sudo chmod 600 /System/Library/CoreServices/NotificationCenter.app/Contents/MacOS/NotificationCenter
	
		launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist
		sudo defaults write /System/Library/LaunchAgents/com.apple.notificationcenterui KeepAlive -bool true
	
		killall NotificationCenter
		killall SystemUIServer
		killall Spotlight
	end
end