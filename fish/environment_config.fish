set -u FISH_PLATFORM_NAME (uname -s)

set -xg EDITOR micro

if test -e $HOME/.go
    set -xg GOPATH $HOME/.go
end

if [ $FISH_PLATFORM_NAME = "Darwin" ]

    # for working modes
    if test (defaults domains | grep "com.pewpewthespells.fish.modes")
        set -u ENABLED_ANDROID (defaults read com.pewpewthespells.fish.modes ENABLE_ANDROID)
        set -u ENABLED_WORK (defaults read com.pewpewthespells.fish.modes ENABLED_WORK)
    else
        set -u ENABLE_ANDROID "false"
        set -u ENABLE_WORK "false"
    end

    if test "$ENABLED_ANDROID" = "true"
        set -xg ANDROID_HOME /usr/local/opt/android-sdk
        set -xg ANDROID_NDK_HOME /usr/local/opt/android-ndk
    else
        set -xg ANDROID_HOME ""
        set -xg ANDROID_NDK_HOME ""
    end
end

