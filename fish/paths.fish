
# setting $GEM_HOME
set LOCAL_GEM_HOME ""
if test -e ~/.gem
	set LOCAL_GEM_HOME ~/.gem
end
set GEM_HOME $GEM_HOME $LOCAL_GEM_HOME

# setting $PYTHONSTARTUP
set PYTHONSTARTUP_PATH ""
if test -e ~/.pythonrc
	set PYTHONSTARTUP_PATH ~/.pythonrc
end
set PYTHONSTARTUP $PYTHONSTARTUP_PATH

# setting up local search paths
set LOCAL_PYTHON_PATH ""
if test -e ~/Library/Python/2.7/bin
	set LOCAL_PYTHON_PATH ~/Library/Python/2.7/bin
end
set LOCAL_RUBY_PATH ""
if test -e  ~/.gem/ruby/2.0.0/bin
	set LOCAL_RUBY_PATH ~/.gem/ruby/2.0.0/bin
end

if test "$ENABLED_WORK" = "true"
	set -xg COLLAB_PATH /Applications/ccollab_client
else
	set -xg COLLAB_PATH ""
end

# setting $PATH
set PATH /usr/local/bin /usr/bin /usr/sbin /bin /sbin $LOCAL_PYTHON_PATH $LOCAL_RUBY_PATH $COLLAB_PATH