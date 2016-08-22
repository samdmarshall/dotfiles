
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


set -xg ANDROID_HOME ""
set -xg ANDROID_NDK_HOME ""
set ANDROID_SDK_PATH ""
set ANDROID_NDK_PATH ""

if test "$ENABLE_ANDROID" = "true"
	if test -e /usr/local/opt/android-sdk
		set -xg ANDROID_HOME /usr/local/opt/android-sdk
		set -xg ANDROID_NDK_HOME /usr/local/opt/android-ndk
	end
end

set CCOLLAB_PATH ""

if test "$ENABLE_WORK" = "true"
	if test -e /Applications/ccollab_client
		set CCOLLAB_PATH /Applications/ccollab_client
	end
end

# setting up local search paths
set LOCAL_PYTHON_PATH ""
if test -e ~/Library/Python/2.7/bin
	set LOCAL_PYTHON_PATH ~/Library/Python/2.7/bin
end
set LOCAL_RUBY_PATH ""
if test -e  ~/.gem/ruby/2.0.0/bin
	set LOCAL_RUBY_PATH ~/.gem/ruby/2.0.0/bin
end
# setting $PATH
set PATH $PATH $LOCAL_PYTHON_PATH $LOCAL_RUBY_PATH $CCOLAB_PATH
