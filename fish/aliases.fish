alias server "ssh -t -t -L 5902:127.0.0.1:5901 samdm@pewpewthespells.com"
alias Pegasus "ssh Pegasus.local"
alias Galactica "ssh Galactica.local"

alias certinfo "openssl x509 -inform DER -text -in"
alias video2gif "ffmpeg -vf scale=640:-1 -gifflags +transdiff ~/Desktop/out.gif -i"

alias svndiff "svn diff --diff-cmd=diff"
alias svncommits "svn log -v --xml | grep '<author./*author>' | sort | uniq -c | sort -rn | sed -e 's=<author>==g' -e 's=</author>==g'"

alias pcat "pygmentize"

alias GetServerLogs "scp -r samdm@pewpewthespells.com:/var/www/pewpewthespells.com/logs/ ~/Sites/; find ~/Sites/logs/ -name '*.gz' | xargs gunzip -f"

alias json "python -m json.tool"

if [ "$FISH_PLATFORM_NAME" = "Darwin" ]
    alias ScreenSaver "sudo open -a ScreenSaverEngine"
    alias bundleid "mdfind kMDItemCFBundleIdentifier = "
    alias show "open --reveal"

    alias disablephotos "defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool YES"
    alias tweetbotdirectlinks "defaults write com.tapbots.TweetbotMac OpenURLsDirectly YES"

    alias BTMM "echo show Setup:/Network/BackToMyMac | scutil | sed -n 's/.* : *\(.*\).\$/\1/p'"
    alias backtohome "ssh -q galactica.(BTMM)"

    alias ppinfo "security cms -D -i"
end

if [ "$FISH_PLATFORM_NAME" = "Linux" ]
    alias UpdateSSLCert "~/letsencrypt/letsencrypt-auto certonly --apache -d pewpewthespells.com,www.pewpewthespells.com"
end

if test -e ~/Sites/markdown
    alias UpdateSite "python $CORE_SCRIPTS_PATH/gensite.py ~/Sites/markdown/sitemap.txt -u"
end
