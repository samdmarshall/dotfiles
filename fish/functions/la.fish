
function la 
  command ls -lahF \
	--ignore="desktop.ini" \
	--ignore="[n|N][t|T][u|U][s|S][e|E][r|R].*" \
	--ignore=".DS_Store" \
	--color=always \
	$argv
end

