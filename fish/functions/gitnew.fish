function gitnew
	git init
	
	cp ~/.config/defaults/clang-format ./.clang-format
	cp ~/.config/defaults/gitignore ./.gitignore
	cp ~/.config/defaults/LICENSE ./LICENSE
	cp ~/.config/defaults/CONTRIBUTING.md ./CONTRIBUTING.md
	
end