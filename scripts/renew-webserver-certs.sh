#!/usr/bin/env sh
sudo service nginx stop
sudo certbot renew --renew-by-default
sudo service nginx start
exit 0
