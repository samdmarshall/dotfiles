alias server "ssh -t -t -L 5902:127.0.0.1:5901 samdm@pewpewthespells.com"
alias home "server 'ssh -L 5901:127.0.0.1:5900 samantha@$HOME_IP'"
alias travel "ssh -L 5901:127.0.0.1:5900 Pegasus.local"
alias Pegasus "ssh Pegasus.local"
alias Galactica "ssh Galactica.local"

alias disablephotos "defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool YES"

if [ $HAS_SCUTIL = true ];
	alias BTMM "echo show Setup:/Network/BackToMyMac | scutil | sed -n 's/.* : *\(.*\).\$/\1/p'"
	alias backtohome "ssh -q galactica.(BTMM)"
end

if [ $HAS_XATTR = true ];
	alias vaccine "xattr -rd com.apple.quarantine" 
end

if [ $HAS_OPENSSL = true ];
	alias certinfo "openssl x509 -inform DER -text -in"
end

if [ $HAS_SECURITY = true ];
	alias ppinfo "security cms -D -i"
end

if [ $HAS_FFMPEG = true ];
	alias video2gif "ffmpeg -vf super2xsai,scale=w=iw/2:h=ih/2 ~/Desktop/out.gif -i"
end

if [ $HAS_WC = true ];
	alias numchar "wc -m"
end

# alias to xcrunner (https://github.com/samdmarshall/xcrunner) or xcrun
if [ $HAS_XCRUNNER = true ];
	alias xc xcrunner
else
	if [ $HAS_XCRUN = true ];
		alias xc xcrun
	end
end

if [ $HAS_GIT = true ];
	alias VisualLog "git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
	alias gitnuke 'git status | grep "deleted:" | awk \'{print $2}\' | xargs git rm'
end

if [ $HAS_SVN = true ];
	alias svndiff "svn diff --diff-cmd=diff"
	alias svncommits "svn log -v --xml | grep '<author./*author>' | sort | uniq -c | sort -rn | sed -e 's=<author>==g' -e 's=</author>==g'"
end

if [ "$PLATFORM_NAME" = "Darwin" ];
	alias ScreenSaver "sudo open -a ScreenSaverEngine"
	alias bundleid "mdfind kMDItemCFBundleIdentifier = "
	alias mkwindow "open -a Finder "
end

if [ $HAS_SHUTDOWN = true ];
	alias HostShutdown "printf '%s%s%s\n' (set_color red) \"WARNING: SHUTTING DOWN HOST NOW! ABORTING WILL EXIT THE SHELL!\" (set_color normal); sudo shutdown -h now; exit"
end

if [ $HAS_PYTHON = true ];
	alias json "python -m json.tool"
	alias tbmute "python $CORE_SCRIPTS_PATH/tweetbot-mute.py"
	if test -e ~/Sites/markdown
		alias UpdateSite "python $CORE_SCRIPTS_PATH/gensite.py ~/Sites/markdown/"
	end
end

if [ $HAS_PERL = true ];
	alias cloc "perl $CORE_SCRIPTS_PATH/cloc.pl"
end

alias GetServerLogs "scp -r samdm@pewpewthespells.com:/var/www/pewpewthespells.com/logs/ ~/Sites/; find ~/Sites/logs/ -name '*.gz' | xargs gunzip -f"
