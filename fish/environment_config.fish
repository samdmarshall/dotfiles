set env_cmd_test_result false
if which security > /dev/null; set env_cmd_test_result true; end
set -xg HAS_SECURITY $env_cmd_test_result

set env_cmd_test_result false
if which git > /dev/null; set env_cmd_test_result true; end
set -xg HAS_GIT $env_cmd_test_result

set env_cmd_test_result false
if which svn > /dev/null; set env_cmd_test_result true; end
set -xg HAS_SVN $env_cmd_test_result

set env_cmd_test_result false
if which scutil > /dev/null; set env_cmd_test_result true; end
set -xg HAS_SCUTIL $env_cmd_test_result

set env_cmd_test_result false
if which xattr > /dev/null; set env_cmd_test_result true; end
set -xg HAS_XATTR $env_cmd_test_result

set env_cmd_test_result false
if which openssl > /dev/null; set env_cmd_test_result true; end
set -xg HAS_OPENSSL $env_cmd_test_result

set env_cmd_test_result false
if which ffmpeg > /dev/null; set env_cmd_test_result true; end
set -xg HAS_FFMPEG $env_cmd_test_result

set env_cmd_test_result false
if which xcrun > /dev/null; set env_cmd_test_result true; end
set -xg HAS_XCRUN $env_cmd_test_result

set env_cmd_test_result false
if which xcrunner > /dev/null; set env_cmd_test_result true; end
set -xg HAS_XCRUNNER $env_cmd_test_result

set env_cmd_test_result false
if which shutdown > /dev/null; set env_cmd_test_result true; end
set -xg HAS_SHUTDOWN $env_cmd_test_result

set env_cmd_test_result false
if which python > /dev/null; set env_cmd_test_result true; end
set -xg HAS_PYTHON $env_cmd_test_result

set env_cmd_test_result false
if which perl > /dev/null; set env_cmd_test_result true; end
set -xg HAS_PERL $env_cmd_test_result

set env_cmd_test_result false
if which wc > /dev/null; set env_cmd_test_result true; end
set -xg HAS_WC $env_cmd_test_result

set env_cmd_test_result false
if which hg > /dev/null; set env_cmd_test_result true; end
set -xg HAS_HG $env_cmd_test_result