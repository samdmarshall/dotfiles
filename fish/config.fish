# disable greeting
set fish_greeting ""

# fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'

function current_working_dir
	#setting up current working dir
	set working_path_sub (pwd | sed -e "s=^$HOME=~=" | awk '{if(length($1)-40 < 0){print substr($1,0,length($1))}else{print "..."substr($1,length($1)-37,length($1))}}')
	echo $working_path_sub;
end

function fish_reload
	. ~/.config/fish/config.fish
end

function fish_pull_update
	set CURR (pwd)
	cd ~/.config
	git pull
	fish_reload
	cd $CURR
end

function fish_push_update
	set CURR (pwd)
	cd ~/.config
	git add -f fish/config.fish fish/functions/*.fish
	if test -e ~/.config/fish/functions/testfunc.fish
		git reset -- fish/functions/testfunc.fish 
	end
	git commit -m "$argv"
	git push origin master
	cd $CURR
end

function fish_prompt
	#setting up colours
	set -g __fish_prompt_user (set_color blue)
	set -g __fish_prompt_host (set_color blue)
	set -g __fish_prompt_normal (set_color normal)
	set -g __fish_prompt_path (set_color $fish_color_cwd)
	#setting up hostname
	set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
	
	
	echo -n -s "$__fish_prompt_user" "$USER" "$__fish_prompt_normal" @ "$__fish_prompt_host" "$__fish_prompt_hostname" ":" "$__fish_prompt_path" (current_working_dir) "$__fish_prompt_normal"
    
	set_color normal
    printf '%s ' (__fish_git_prompt)
    set_color normal
	
	echo -n -s '$ '
	
end

# setting up local search paths
set XCODE_NODE_PATH ""
if test -e /Applications/Xcode.app/Contents/Developer/usr/share/xcs/Node/bin
	set XCODE_NODE_PATH /Applications/Xcode.app/Contents/Developer/usr/share/xcs/Node/bin
end
set LOCAL_PYTHON_PATH ""
if test -e ~/Library/Python/2.7/bin
	set LOCAL_PYTHON_PATH ~/Library/Python/2.7/bin
end
set LOCAL_RUBY_PATH ""
if test -e  ~/.gem/ruby/2.0.0/bin
	set LOCAL_RUBY_PATH ~/.gem/ruby/2.0.0/bin
end
set MACPORTS_BIN ""
if test -e /opt/local/bin
	set MACPORTS_BIN /opt/local/bin
end
set MACPORTS_SBIN ""
if test -e /opt/local/sbin
	set MACPORTS_SBIN /opt/local/sbin
end
# setting $PATH
set PATH $MACPORTS_BIN $MACPORTS_SBIN $PATH $LOCAL_PYTHON_PATH $LOCAL_RUBY_PATH $XCODE_NODE_PATH

# setting $GEM_HOME
set LOCAL_GEM_HOME ""
if test -e ~/.gems
	set LOCAL_GEM_HOME ~/.gems
end
set GEM_HOME $GEM_HOME $LOCAL_GEM_HOME