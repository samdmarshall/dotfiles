function d
	command tree -a -C -I ".git" -I ".svn" -I ".hg" --noreport -F $argv | eval $PAGER
end
