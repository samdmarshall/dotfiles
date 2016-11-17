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
