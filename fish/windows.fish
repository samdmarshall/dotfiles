
set --export --global DOCKER_HOST "tcp://172.20.247.219:2376"
set --export --global DOCKER_CERT_PATH "/mnt/c/Users/Demi/.docker/machine/machines/default/"
set --export --global COMPOSE_CONVERT_WINDOWS_PATHS true


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

