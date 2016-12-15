alias server "ssh -t -t -L 5902:127.0.0.1:5901 samdm@pewpewthespells.com"
alias Pegasus "ssh Pegasus.local"
alias Galactica "ssh Galactica.local"

alias json "python -m json.tool"

alias vcs "printf '%s\n' (__fish_vcs_prompt)"


if [ "$FISH_PLATFORM_NAME" = "Darwin" ]
    alias ScreenSaver "sudo open -a ScreenSaverEngine"

    alias disablephotos "defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool YES"

    alias BTMM "echo show Setup:/Network/BackToMyMac | scutil | sed -n 's/.* : *\(.*\).\$/\1/p'"
    alias backtohome "ssh -q galactica.(BTMM)"
end

if [ "$FISH_PLATFORM_NAME" = "Linux" ]
    alias UpdateSSLCert "~/letsencrypt/letsencrypt-auto certonly --apache -d pewpewthespells.com,www.pewpewthespells.com"
end
