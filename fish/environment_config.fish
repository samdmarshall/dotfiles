set -x PLATFORM_NAME (uname -s)

if [ $PLATFORM_NAME = "Darwin" ];
	if not set -q HAS_SECURITY
		set env_cmd_test_result false
		if which security > /dev/null; set env_cmd_test_result true; end
		set -xg HAS_SECURITY $env_cmd_test_result
	end
	
	if not set -q HAS_SCUTIL
		set env_cmd_test_result false
		if which scutil > /dev/null; set env_cmd_test_result true; end
		set -xg HAS_SCUTIL $env_cmd_test_result
	end
	
	if not set -q HAS_XCRUN
		set env_cmd_test_result false
		if which xcrun > /dev/null; set env_cmd_test_result true; end
		set -xg HAS_XCRUN $env_cmd_test_result
	end

	if not set -q HAS_XCRUNNER
		set env_cmd_test_result false
		if which xcrunner > /dev/null; set env_cmd_test_result true; end
		set -xg HAS_XCRUNNER $env_cmd_test_result
	end
	
	if not set -q HAS_XATTR
		set env_cmd_test_result false
		if which xattr > /dev/null; set env_cmd_test_result true; end
		set -xg HAS_XATTR $env_cmd_test_result
	end
	
	if not set -q HAS_DEFAULTS
		set env_cmd_test_result false
		if which defaults > /dev/null; set env_cmd_test_result true; end
		set -xg HAS_DEFAULTS $env_cmd_test_result
	end
end

if not set -q HAS_GIT
	set env_cmd_test_result false
	if which git > /dev/null; set env_cmd_test_result true; end
	set -xg HAS_GIT $env_cmd_test_result
end

if not set -q HAS_SVN
	set env_cmd_test_result false
	if which svn > /dev/null; set env_cmd_test_result true; end
	set -xg HAS_SVN $env_cmd_test_result
end

if not set -q HAS_OPENSSL
	set env_cmd_test_result false
	if which openssl > /dev/null; set env_cmd_test_result true; end
	set -xg HAS_OPENSSL $env_cmd_test_result
end

if not set -q HAS_FFMPEG
	set env_cmd_test_result false
	if which ffmpeg > /dev/null; set env_cmd_test_result true; end
	set -xg HAS_FFMPEG $env_cmd_test_result
end

if not set -q HAS_SHUTDOWN
	set env_cmd_test_result false
	if which shutdown > /dev/null; set env_cmd_test_result true; end
	set -xg HAS_SHUTDOWN $env_cmd_test_result
end

if not set -q HAS_PYTHON
	set env_cmd_test_result false
	if which python > /dev/null; set env_cmd_test_result true; end
	set -xg HAS_PYTHON $env_cmd_test_result
end

if not set -q HAS_WC
	set env_cmd_test_result false
	if which wc > /dev/null; set env_cmd_test_result true; end
	set -xg HAS_WC $env_cmd_test_result
end

if not set -q HAS_HG
	set env_cmd_test_result false
	if which hg > /dev/null; set env_cmd_test_result true; end
	set -xg HAS_HG $env_cmd_test_result
end