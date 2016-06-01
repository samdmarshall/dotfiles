# fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'

function prompt_current_working_dir
	set should_show_path true
	set path_prefix ""
	set working_path (pwd | sed -e "s=^$HOME=~=")
	set default_length "40"
	set total_length "38"
	if [ $HAS_WC = true ];
		#setting up current working dir for path truncation
		set working_path_length (echo -n $working_path | numchar)
		set user_host_length (math "$prompt_user_length + 1 + $prompt_hostname_length + 1")
		if [ $user_host_length -ge $default_length ];
			set should_show_path false
		end
		set total_length (math "$default_length - $user_host_length")
		if [ $working_path_length -gt $total_length ];
			set total_length (math "$total_length - 3")
			set path_prefix "..."
		end
	end
	set working_path_sub (echo -n $working_path | tail -c $total_length)
	if [ $should_show_path = false ];
		set working_path_sub ""
	end
	echo -n $path_prefix$working_path_sub
end

function fish_right_prompt
	printf '%s' (__fish_vcs_prompt)
end

function fish_prompt
	# setting up colours
	if not set -q __fish_prompt_user
		set -g __fish_prompt_user (set_color purple)
	end
	if not set -q __fish_prompt_host
		set -g __fish_prompt_host (set_color cyan)
	end
	if not set -q __fish_prompt_normal
		set -g __fish_prompt_normal (set_color normal)
	end
	if not set -q __fish_prompt_path
		set -g __fish_prompt_path (set_color green)
	end
	
	# setting up hostname
	if not set -q __fish_prompt_hostname
		set -g __fish_prompt_hostname (hostname -s)
	end
	if [ $HAS_WC = true ];
		if not set -q prompt_hostname_length
			set -g prompt_hostname_length (echo -n $__fish_prompt_hostname | numchar)
		end
		if not set -q prompt_user_length
			set -g prompt_user_length (echo -n $USER | numchar)
		end
	end
	
	# printing prompt
	printf '%s' $__fish_prompt_user
	printf '%s' $USER
	printf '%s' $__fish_prompt_normal
	printf '@'
	printf '%s' $__fish_prompt_host
	printf '%s' $__fish_prompt_hostname
	printf '%s' $__fish_prompt_normal
	printf ':'
	printf '%s' $__fish_prompt_path
	printf '%s' (prompt_current_working_dir)
	printf '%s' $__fish_prompt_normal
	printf ' $ '	
end
