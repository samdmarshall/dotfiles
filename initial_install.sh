#!/usr/bin/env sh

touch ~/.hushlogin

ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

pushd ~/.config/defaults
brew bundle
sudo easy_install pip
pip install -r requirements.txt --user
popd

sudo xcode-select --install

cp ~/.config/lldb/lldbinit ~/.lldbinit

mkdir -p ~/Documents/email/
mkdir -p ~/Documents/email/cur/
mkdir -p ~/Documents/email/new/
mkdir -p ~/Documents/email/tmp/
