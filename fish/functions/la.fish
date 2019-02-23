
function la 
  command ls -lahF \
	--ignore="desktop.ini" \
	--ignore="[n|N][t|T][u|U][s|S][e|E][r|R].*" \
	--ignore=".DS_Store" \
	--ignore="Default.rdp" \
	--color=always \
	$argv
end

