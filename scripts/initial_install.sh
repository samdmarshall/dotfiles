#!/bin/sh

sudo ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew install fish
brew install ffmpeg
brew install pandoc
brew install nmap
brew install wget
brew install clang-format
brew install goaccess --with-geoip
brew install john
brew install hydra
brew install git
brew install cmake 
brew install vbindiff
brew install binutils
brew install automake
brew install autoconf
brew install lynx

sudo easy_install pip

sudo pip install paramiko
sudo pip install scp
sudo pip install machobot
sudo pip install thefuck

sudo ~/.config/lldb/install.sh


sudo cat <<EOT >> /usr/local/bin/git-xcode
#!/usr/bin/env sh

read -r -d '' script <<'EOF'
set working_dir_path to (do shell script "echo $PWD")
set working_dir to POSIX file working_dir_path

tell application "Xcode"
	repeat with current_window in windows
		repeat with workspace in documents of current_window
			set workspace_name to name of workspace
			set the item_list to list folder working_dir without invisibles
			if item_list contains workspace_name then
				set active_files to {}
				repeat while (count of (source documents whose path contains working_dir_path)) > 0
					set active_documents to (source documents whose path contains working_dir_path)
					repeat with source_document in active_documents
						copy path of source_document to end of active_files
						close source_document without saving
					end repeat
				end repeat
				set active_files to reverse of active_files
				repeat with file_path in active_files
					do shell script "open -b com.apple.dt.Xcode \"" & file_path & "\""
				end repeat
			end if
		end repeat
	end repeat
end tell
EOF

osascript -e "$script"
EOT

sudo chmod +x /usr/local/bin/git-xcode