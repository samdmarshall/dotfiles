alias server "ssh -t -t -L 5902:127.0.0.1:5901 samdm@pewpewthespells.com"
alias home "server 'ssh -L 5901:127.0.0.1:5900 samantha@$HOME_IP'"
alias travel "ssh -L 5901:127.0.0.1:5900 Pegasus.local"
alias Pegasus "ssh Pegasus.local"
alias Galactica "ssh Galactica.local"

if which scutil > /dev/null 
	alias BTMM "echo show Setup:/Network/BackToMyMac | scutil | sed -n 's/.* : *\(.*\).\$/\1/p'"
	alias backtohome "ssh galactica.(BTMM)"
	alias homeproxy "backtohome -D 1234"
end

if which xattr > /dev/null
	alias vaccine "xattr -rd com.apple.quarantine" 
end

if which openssl > /dev/null
	alias certinfo "openssl x509 -inform DER -text -in"
end

if which security > /dev/null
	alias ppinfo "security cms -D -i"
end

if which ffmpeg > /dev/null
	alias video2gif "ffmpeg -vf super2xsai,scale=w=iw/2:h=ih/2 ~/Desktop/out.gif -i"
end


# alias to xcrunner (https://github.com/samdmarshall/xcrunner) or xcrun
if which xcrunner > /dev/null
	alias xc xcrunner
else
	if which xcrun > /dev/null
		alias xc xcrun
	end
end


alias VisualLog "git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

if test "$PLATFORM_NAME" = "Darwin"
	alias ScreenSaver "sudo open -a ScreenSaverEngine"
	alias bundleid "mdfind kMDItemCFBundleIdentifier = "
	alias mkwindow "open -a Finder ."
end

if which shutdown > /dev/null
	alias HostShutdown "printf '%s%s%s\n' (set_color red) \"WARNING: SHUTTING DOWN HOST NOW! ABORTING WILL EXIT THE SHELL!\" (set_color normal); sudo shutdown -h now; exit"
end

alias json "python -m json.tool"

alias cloc "perl $CORE_SCRIPTS_PATH/cloc.pl"

alias tbmute "python $CORE_SCRIPTS_PATH/tweetbot-mute.py"

if test -e ~/Sites/markdown
	alias UpdateSite "python $CORE_SCRIPTS_PATH/gensite.py ~/Sites/markdown/"
end

alias GetServerLogs "scp -r samdm@pewpewthespells.com:/var/www/pewpewthespells.com/logs/ ~/Sites/; find ~/Sites/logs/ -name '*.gz' | xargs gunzip -f"

alias gitnuke='git status | grep "deleted:" | awk \'{print $2}\' | xargs git rm'