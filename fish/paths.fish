
# setting $GEM_HOME
set -u LOCAL_GEM_HOME ""
if test -e ~/.gem
    set LOCAL_GEM_HOME ~/.gem
end
set GEM_HOME $GEM_HOME $LOCAL_GEM_HOME

# setting $PYTHONSTARTUP
set -u PYTHONSTARTUP_PATH ""
if test -e ~/.pythonrc
    set PYTHONSTARTUP_PATH ~/.pythonrc
end
set PYTHONSTARTUP $PYTHONSTARTUP_PATH

# setting up local search paths
set -u LOCAL_PYTHON_PATH (python -m site --user-base)"/bin"
set -u LOCAL_RUBY_PATH (gem environment gempath | sed -e 's=:.*$=/bin=')

if test "$ENABLED_WORK" = "true"
    set -u COLLAB_PATH /Applications/ccollab_client
else
    set -u COLLAB_PATH ""
end

# setting $PATH
set PATH $PATH $LOCAL_PYTHON_PATH $LOCAL_RUBY_PATH $COLLAB_PATH $CORE_SCRIPTS_PATH
