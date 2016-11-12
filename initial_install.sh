#!/usr/bin/env sh

touch ~/.hushlogin

ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

pushd ~/.config/defaults
brew bundle
popd

sudo easy_install pip

sudo pip install paramiko
sudo pip install scp
sudo pip install machobot

sudo xcode-select --install

cp ~/.config/lldb/lldbinit ~/.lldbinit
