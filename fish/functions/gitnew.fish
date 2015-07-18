function gitnew

	if which git > /dev/null
		git init
	
		cp $GIT_DEFAULTS_DIR/clang-format ./.clang-format
		cp $GIT_DEFAULTS_DIR/gitignore ./.gitignore
		cp $GIT_DEFAULTS_DIR/LICENSE ./LICENSE
		cp $GIT_DEFAULTS_DIR/CONTRIBUTING.md ./CONTRIBUTING.md
	else
		echo "git isn't installed!"
	end
	
end