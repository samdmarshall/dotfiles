
set --export --global DOCKER_HOST "tcp://127.0.0.1:2375"
set --export --global DOCKER_CERT_PATH "/mnt/c/Users/Demi/.docker/machine/machines/default/"
set --export --global COMPOSE_CONVERT_WINDOWS_PATHS true

alias code "/mnt/c/Users/Demi/AppData/Local/Programs/Microsoft\ VS\ Code/Code.exe"

# This is to automatically start specific daemons in WSL since it doesn't auto-start background services
if contains "service" (ls /etc/sudoers.d/) 
	if test (count (ps -C avahi-daemon -o pid=)) = 0
		sudo service dbus start > /dev/null
		sudo service avahi-daemon start > /dev/null # start service to allow for bonjour network lookups
	end
else
	echo "Please add entry for 'service' in '/etc/sudoers.d/'!"
	echo "  $USER ALL=(root) NOPASSWD: /usr/sbin/service"
end

if test (count (ls ~/Documents)) = 0
	sudo mount --bind /mnt/c/Users/Demi/Documents ~/Documents
end

if test (count (ls ~/Downloads)) = 0
	sudo mount --bind /mnt/c/Users/Demi/Downloads ~/Downloads
end

if test (count (ls ~/Projects)) = 0
	sudo mount --bind /mnt/c/Users/Demi/Development ~/Projects
end

if test (count (ls ~/iCloud)) = 0
	sudo mount --bind /mnt/c/Users/Demi/iCloudDrive/ ~/iCloud
end

if test (count (ls ~/Pictures)) = 0
	sudo mount --bind /mnt/c/Users/Demi/Pictures/ ~/Pictures
end

if test (count (ls ~/Music)) = 0
	sudo mount --bind /mnt/c/Users/Demi/Music/ ~/Music
end

if test (count (ls ~/Videos)) = 0
	sudo mount --bind /mnt/c/Users/Demi/Videos/ ~/Videos
end

if test (count (ls ~/Desktop)) = 0
	sudo mount --bind /mnt/c/Users/Demi/Desktop/ ~/Desktop
end

if test (count (ls ~/.vscode)) = 0
	sudo mount --bind /mnt/c/Users/Demi/.vscode/ ~/.vscode
end
