umask 002

# =====================================
# Setup directories shared from Windows
# =====================================

set --local --unexport sudoers_mount_entry (test -f /etc/sudoers.d/mount; echo $status)

if test $sudoers_mount_entry = 0
	function mount_directory --argument-names local remote
		if test (count $local/*) = 0
			sudo mount --bind $remote $local
		end
	end

	mount_directory ~/Documents $WIN_HOME/Documents
	mount_directory ~/Downloads $WIN_HOME/Downloads
	mount_directory ~/Projects  $WIN_HOME/Development
	mount_directory ~/iCloud    $WIN_HOME/iCloudDrive
	mount_directory ~/Pictures  $WIN_HOME/Pictures
	mount_directory ~/Music     $WIN_HOME/Music
	mount_directory ~/Videos    $WIN_HOME/Videos
	mount_directory ~/Desktop   $WIN_HOME/Desktop
	mount_directory ~/.docker   $WIN_HOME/.docker

else
	echo "Please add entry for 'mount' in '/etc/sudoers.d/'!"
	echo "  $USER ALL=(root) NOPASSWD: /bin/mount"
end


# ======================
# Set Starting Directory
# ======================

if test "$SHLVL" -ne 1
  exit 1
end

switch $PWD
  case /mnt/c/Windows/System32 $WIN_HOME
    cd $HOME
end
