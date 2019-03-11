umask 002


# =======================
# System Services Startup
# =======================

if test -e /etc/sudoers.d/service
	if test (count (pgrep "avahi-daemon")) = 0
		# start services to allow for bonjour network lookups
		sudo service dbus start > /dev/null
		sudo service avahi-daemon start > /dev/null
	end
else
	echo "Please add entry for 'service' in '/etc/sudoers.d/'!"
	echo "  $USER ALL=(root) NOPASSWD: /usr/sbin/service"
end

#set --unexport emacs_daemon_pid (pgrep "emacs --daemon --user demi")
#if test -z $emacs_daemon_pid 
#	emacs --daemon --user demi > /dev/null
#end


# =====================================
# Setup directories shared from Windows
# =====================================

if test -e /etc/sudoers.d/mount
	function mount_directory --argument-names local remote
		if test (count $local/*) = 0
			sudo mount --bind $remote $local
		end
	end

	mount_directory ~/Documents /mnt/c/Users/Demi/Documents
	mount_directory ~/Downloads /mnt/c/Users/Demi/Downloads
	mount_directory ~/Projects  /mnt/c/Users/Demi/Development
	mount_directory ~/iCloud    /mnt/c/Users/Demi/iCloudDrive
	mount_directory ~/Pictures  /mnt/c/Users/Demi/Pictures
	mount_directory ~/Music     /mnt/c/Users/Demi/Music
	mount_directory ~/Videos    /mnt/c/Users/Demi/Videos
	mount_directory ~/Desktop   /mnt/c/Users/Demi/Desktop
	mount_directory ~/.docker   /mnt/c/Users/Demi/.docker
else
	echo "Please add entry for 'mount' in '/etc/sudoers.d/'!"
	echo "  $USER ALL=(root) NOPASSWD: /bin/mount"
end


# ======================
# Set Starting Directory
# ======================

if test $SHLVL -ne 1
  exit 1
end
switch $PWD
  case /mnt/c/Windows/System32
	  echo "Started as Windows Administrator"
	  cd ~
  case /mnt/c/Users/*
    set --unexport --local split_path (string split "/" $PWD)
    set --unexport --local user_name_index (math (contains --index "users" $split_path) + 1)
    if test user_name_index -lt (count $split_path)
      echo "Started as Windows User: " $split_path[$user_name_index]
    else
      echo "Started as Unknown Windows User"
    end
	  cd ~
end
