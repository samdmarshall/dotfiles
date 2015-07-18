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
	#setting up current working dir for path truncation
	set working_path_sub (pwd | sed -e "s=^$HOME=~=" | awk -F':' -v user_name=$USER -v host_name=$__fish_prompt_hostname '{
		total_length=40-length(user_name)-2-length(host_name);
		elipse_length=3;
		trunc_length=total_length-elipse_length;
		
		if (length($1) - total_length < 0) {
			print substr($1,0,length($1))
		}
		else {
			print "..."substr($1,length($1)-trunc_length,length($1))
		}
	}')
	echo $working_path_sub;
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
		set -g __fish_prompt_hostname (hostname | cut -d . -f 1)
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
	printf '%s ' (__fish_git_prompt)
	printf '%s' $__fish_prompt_normal
	printf '$ '	
end