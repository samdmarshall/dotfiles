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