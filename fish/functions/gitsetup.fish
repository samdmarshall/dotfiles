function gitsetup
	
	set filename "$PWD/.git"
	
	if test -d $filename
		echo ""
	else
		gitnew
	end
	
	set arg_count (echo $argv | wc -w | awk '{print $1}')
	
	if [ $arg_count -eq 1 ];
		set remote "$argv[1]"
		
		git remote add origin "$remote"
		git checkout -b develop
		git add .clang-format .gitignore CONTRIBUTING.md LICENSE
		git commit -m "Initial Setup"
		git push -u origin develop
		
	else
		echo "Please pass a video file path"
	end
	
end