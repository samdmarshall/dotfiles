function getweblog
	set CURR (pwd)
	
	scp -r samdm@pewpewthespells.com:/var/www/pewpewthespells.com/logs/ ~/Sites/logs/
	
	find ~/Sites/logs/ -name "*.gz" | xargs gunzip -f
	
	cd $CURR
end