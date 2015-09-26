alias server "ssh -t -t -L 5902:127.0.0.1:5901 samdm@pewpewthespells.com"
alias home "server 'ssh -L 5901:127.0.0.1:5900 samantha@$HOME_IP'"
alias travel "ssh -L 5901:127.0.0.1:5900 Pegasus.local"
alias Pegasus "ssh Pegasus.local"
alias Galactica "ssh Galactica.local"

if test $HAS_SCUTIL
	alias BTMM "echo show Setup:/Network/BackToMyMac | scutil | sed -n 's/.* : *\(.*\).\$/\1/p'"
	alias backtohome "ssh galactica.(BTMM)"
	alias homeproxy "backtohome -D 1234"
end

if test $HAS_XATTR
	alias vaccine "xattr -rd com.apple.quarantine" 
end

if test $HAS_OPENSSL
	alias certinfo "openssl x509 -inform DER -text -in"
end

if test $HAS_SECURITY
	alias ppinfo "security cms -D -i"
end

if test $HAS_FFMPEG
	alias video2gif "ffmpeg -vf super2xsai,scale=w=iw/2:h=ih/2 ~/Desktop/out.gif -i"
end


# alias to xcrunner (https://github.com/samdmarshall/xcrunner) or xcrun
if test $HAS_XCRUNNER
	alias xc xcrunner
else
	if test $HAS_XCRUN
		alias xc xcrun
	end
end

if test $HAS_GIT
	alias VisualLog "git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
	alias gitnuke 'git status | grep "deleted:" | awk \'{print $2}\' | xargs git rm'
end

if test "$PLATFORM_NAME" = "Darwin"
	alias ScreenSaver "sudo open -a ScreenSaverEngine"
	alias bundleid "mdfind kMDItemCFBundleIdentifier = "
	alias mkwindow "open -a Finder ."
end

if test $HAS_SHUTDOWN
	alias HostShutdown "printf '%s%s%s\n' (set_color red) \"WARNING: SHUTTING DOWN HOST NOW! ABORTING WILL EXIT THE SHELL!\" (set_color normal); sudo shutdown -h now; exit"
end

if test $HAS_PYTHON
	alias json "python -m json.tool"
	alias tbmute "python $CORE_SCRIPTS_PATH/tweetbot-mute.py"
	if test -e ~/Sites/markdown
		alias UpdateSite "python $CORE_SCRIPTS_PATH/gensite.py ~/Sites/markdown/"
	end
end

if test $HAS_PERL
	alias cloc "perl $CORE_SCRIPTS_PATH/cloc.pl"
end

alias GetServerLogs "scp -r samdm@pewpewthespells.com:/var/www/pewpewthespells.com/logs/ ~/Sites/; find ~/Sites/logs/ -name '*.gz' | xargs gunzip -f"