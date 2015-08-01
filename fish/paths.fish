# setting up local search paths
set XCODE_NODE_PATH ""
if test -e /Applications/Xcode.app/Contents/Developer/usr/share/xcs/Node/bin
	set XCODE_NODE_PATH /Applications/Xcode.app/Contents/Developer/usr/share/xcs/Node/bin
end
set LOCAL_PYTHON_PATH ""
if test -e ~/Library/Python/2.7/bin
	set LOCAL_PYTHON_PATH ~/Library/Python/2.7/bin
end
set LOCAL_RUBY_PATH ""
if test -e  ~/.gem/ruby/2.0.0/bin
	set LOCAL_RUBY_PATH ~/.gem/ruby/2.0.0/bin
end
set TEXBIN_PATH ""
if test -e /usr/texbin
	set TEXBIN_PATH /usr/texbin
end
# setting $PATH
set PATH $PATH $LOCAL_PYTHON_PATH $LOCAL_RUBY_PATH $XCODE_NODE_PATH $TEXBIN_PATH

# setting $GEM_HOME
set LOCAL_GEM_HOME ""
if test -e ~/.gems
	set LOCAL_GEM_HOME ~/.gems
end
set GEM_HOME $GEM_HOME $LOCAL_GEM_HOME

# setting $PYTHONSTARTUP
set PYTHONSTARTUP_PATH ""
if test -e ~/.pythonrc
	set PYTHONSTARTUP_PATH ~/.pythonrc
end
set PYTHONSTARTUP $PYTHONSTARTUP_PATH