function fixmenubar
	sudo chmod 600 /System/Library/CoreServices/Search.bundle/Contents/MacOS/Search
	sudo chmod 600 /System/Library/CoreServices/NotificationCenter.app/Contents/MacOS/NotificationCenter
	killall NotificationCenter
	killall SystemUIServer
end