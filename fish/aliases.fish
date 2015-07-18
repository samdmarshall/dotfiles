alias server "ssh -t -t -L 5902:127.0.0.1:5901 samdm@pewpewthespells.com"
alias home "server 'ssh -L 5901:127.0.0.1:5900 samantha@$HOME_IP'"
alias Pegasus "ssh Pegasus.local"
alias Galactica "ssh Galactica.local"

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


alias mkwindow "open -a Finder ."

alias gitnuke "git status | grep 'deleted:' | awk '{print $2}' | xargs git rm"

alias json "python -m json.tool"

alias cloc "perl $CORE_SCRIPTS_PATH/cloc.pl"

alias bundleid "mdfind kMDItemCFBundleIdentifier = "

alias tbmute "python $CORE_SCRIPTS_PATH/tweetbot-mute.py"

if test -e ~/Sites/markdown
	alias UpdateSite "python $CORE_SCRIPTS_PATH/gensite.py ~/Sites/markdown/"
end