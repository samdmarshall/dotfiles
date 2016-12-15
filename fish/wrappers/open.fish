function show --wraps=open
    command open --reveal $argv
end

function screensaver --wraps=open
	command sudo open -a ScreenSaverEngine
end