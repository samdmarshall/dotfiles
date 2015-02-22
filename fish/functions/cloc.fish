function cloc
	set arg_count (echo $argv | wc -w | awk '{print $1}')
	
	if [ $arg_count -eq 1 ];
		perl ~/.config/fish/functions/perl/cloc.pl $argv
	else
		perl ~/.config/fish/functions/perl/cloc.pl (pwd)
	end
	
end