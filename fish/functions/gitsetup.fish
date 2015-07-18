function gitsetup --argument remote
	
	set filename "$PWD/.git"
	
	if test -d $filename
		echo "Skipping git-init..."
	else
		gitnew
	end
	
	set arg_count (echo $argv | wc -w | awk '{print $1}')
	
	if [ $arg_count -eq 1 ];
		if which git > /dev/null
			git remote add origin "$remote"
			git checkout -b develop
			git add .clang-format .gitignore CONTRIBUTING.md LICENSE
			git commit -m "Initial Setup"
			git push -u origin develop
		else
			echo "git isn't installed!"
		end
	else
		echo "usage: gitsetup [origin url]"
	end
	
end