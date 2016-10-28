set -u FISH_PLATFORM_NAME (uname -s)

if [ $FISH_PLATFORM_NAME = "Darwin" ];
	if not set -q HAS_SECURITY
		set env_cmd_test_result false
		if command -s security > /dev/null; set env_cmd_test_result true; end
		set -u HAS_SECURITY $env_cmd_test_result
	end
	
	if not set -q HAS_SCUTIL
		set env_cmd_test_result false
		if command -s scutil > /dev/null; set env_cmd_test_result true; end
		set -u HAS_SCUTIL $env_cmd_test_result
	end
	
	if not set -q HAS_XCRUN
		set env_cmd_test_result false
		if command -s xcrun > /dev/null; set env_cmd_test_result true; end
		set -u HAS_XCRUN $env_cmd_test_result
	end

	if not set -q HAS_XCRUNNER
		set env_cmd_test_result false
		if command -s xcrunner > /dev/null; set env_cmd_test_result true; end
		set -u HAS_XCRUNNER $env_cmd_test_result
	end
	
	if not set -q HAS_XATTR
		set env_cmd_test_result false
		if command -s xattr > /dev/null; set env_cmd_test_result true; end
		set -u HAS_XATTR $env_cmd_test_result
	end
	
	if not set -q HAS_DEFAULTS
		set env_cmd_test_result false
		if command -s defaults > /dev/null; set env_cmd_test_result true; end
		set -u HAS_DEFAULTS $env_cmd_test_result
	end

    # for working modes
    set -u ENABLED_ANDROID (defaults read com.pewpewthespells.fish.modes ENABLE_ANDROID)
    set -u ENABLED_WORK (defaults read com.pewpewthespells.fish.modes ENABLED_WORK)

    if test "$ENABLED_ANDROID" = "true"
	    set -xg ANDROID_HOME /usr/local/opt/android-sdk
	    set -xg ANDROID_NDK_HOME /usr/local/opt/android-ndk
    else
	    set -xg ANDROID_HOME ""
	    set -xg ANDROID_NDK_HOME ""
    end
end

if not set -q HAS_GIT
	set env_cmd_test_result false
	if command -s git > /dev/null; set env_cmd_test_result true; end
	set -u HAS_GIT $env_cmd_test_result
end

if not set -q HAS_SVN
	set env_cmd_test_result false
	if command -s svn > /dev/null; set env_cmd_test_result true; end
	set -u HAS_SVN $env_cmd_test_result
end

if not set -q HAS_OPENSSL
	set env_cmd_test_result false
	if command -s openssl > /dev/null; set env_cmd_test_result true; end
	set -u HAS_OPENSSL $env_cmd_test_result
end

if not set -q HAS_FFMPEG
	set env_cmd_test_result false
	if command -s ffmpeg > /dev/null; set env_cmd_test_result true; end
	set -u HAS_FFMPEG $env_cmd_test_result
end

if not set -q HAS_SHUTDOWN
	set env_cmd_test_result false
	if command -s shutdown > /dev/null; set env_cmd_test_result true; end
	set -u HAS_SHUTDOWN $env_cmd_test_result
end

if not set -q HAS_PYTHON
	set env_cmd_test_result false
	if command -s python > /dev/null; set env_cmd_test_result true; end
	set -u HAS_PYTHON $env_cmd_test_result
end

if not set -q HAS_WC
	set env_cmd_test_result false
	if command -s wc > /dev/null; set env_cmd_test_result true; end
	set -u HAS_WC $env_cmd_test_result
end

if not set -q HAS_HG
	set env_cmd_test_result false
	if command -s hg > /dev/null; set env_cmd_test_result true; end
	set -u HAS_HG $env_cmd_test_result
end

