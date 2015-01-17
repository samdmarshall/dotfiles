#!/bin/sh

curr=`pwd`
mkdir -p ~/Projects
cd ~/Projects

if which git > /dev/null; then
	# setting up dbscript
	git clone git@github.com:samdmarshall/lldbscript.git lldbscript
	# setting up voltron
	git clone git@github.com:snare/voltron.git voltron
	cd voltron
	python setup.py install
else
	echo "please install git first!"
fi

cd DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cp lldbinit ~/.lldbinit

cd $curr