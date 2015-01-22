function getweblog
	set CURR (pwd)
	
	scp -r samdm@samdmarshall.com:/var/www/samdmarshall.com/logs/ ~/Sites/
	
	find ~/Sites/logs/ -name "*.gz" | xargs gunzip -f
	
	cd $CURR
end