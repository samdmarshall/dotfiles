#!/usr/bin/env sh

touch ~/.hushlogin

ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

pushd ~/.config/defaults
brew bundle
sudo easy_install pip
pip install -r requirements.txt --user
pip3 install -r requirements.txt
gem install bundler --user-install
popd

sudo xcode-select --install

cp ~/.config/defaults/lldbinit ~/.lldbinit
cp ~/.config/defaults/mailcap  ~/.mailcap
cp ~/.config/defaults/pypirc   ~/.pypirc


mkdir -p ~/eMail/
mkdir -p ~/eMail/cur/
mkdir -p ~/eMail/new/
mkdir -p ~/eMail/tmp/

mkdir -p ~/Calendars/
mkdir -p ~/Calendars/icloud/
mkdir -p ~/Calendars/ellen/
mkdir -p ~/Calendars/cocoaheads/

for file in `ls ~/.config/defaults/LaunchAgents/`; do
    cp ~/.config/defaults/LaunchAgents/$file ~/Library/LaunchAgents/
    launchctl load ~/Library/LaunchAgents/$file
done

chsh -s /usr/local/bin/fish

