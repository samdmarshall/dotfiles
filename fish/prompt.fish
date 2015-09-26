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


## SVN 
set -g __fish_svn_prompt_added_char 'A'
set -g __fish_svn_prompt_added_color green
set -g __fish_svn_prompt_added_display 'A'

set -g __fish_svn_prompt_conflicted_char 'C'
set -g __fish_svn_prompt_conflicted_color --underline magenta
set -g __fish_svn_prompt_conflicted_display 'C'

set -g __fish_svn_prompt_deleted_char 'D'
set -g __fish_svn_prompt_deleted_color red
set -g __fish_svn_prompt_deleted_display 'D'

set -g __fish_svn_prompt_ignored_char 'I'
set -g __fish_svn_prompt_ignored_color --bold yellow
set -g __fish_svn_prompt_ignored_display 'I'

set -g __fish_svn_prompt_modified_char 'M'
set -g __fish_svn_prompt_modified_color blue
set -g __fish_svn_prompt_modified_display 'M'

set -g __fish_svn_prompt_replaced_char 'R'
set -g __fish_svn_prompt_replaced_color cyan
set -g __fish_svn_prompt_replaced_display 'R'

set -g __fish_svn_prompt_unversioned_external_char 'X'
set -g __fish_svn_prompt_unversioned_external_color --underline cyan
set -g __fish_svn_prompt_unversioned_external_display 'X'

set -g __fish_svn_prompt_unversioned_char '?'
set -g __fish_svn_prompt_unversioned_color purple
set -g __fish_svn_prompt_unversioned_display '?'

set -g __fish_svn_prompt_missing_char '!'
set -g __fish_svn_prompt_missing_color yellow
set -g __fish_svn_prompt_missing_display '!'

set -g __fish_svn_prompt_versioned_obstructed_char '~'
set -g __fish_svn_prompt_versioned_obstructed_color magenta
set -g __fish_svn_prompt_versioned_obstructed_display '~'

set -g __fish_svn_prompt_locked_char 'L'
set -g __fish_svn_prompt_locked_color --bold red
set -g __fish_svn_prompt_locked_display 'L'

set -g __fish_svn_prompt_scheduled_char '+'
set -g __fish_svn_prompt_scheduled_color --bold green
set -g __fish_svn_prompt_scheduled_display '+'

set -g __fish_svn_prompt_switched_char 'S'
set -g __fish_svn_prompt_switched_color --bold blue
set -g __fish_svn_prompt_switched_display 'S'

set -g __fish_svn_prompt_token_present_char 'K'
set -g __fish_svn_prompt_token_present_color --bold cyan
set -g __fish_svn_prompt_token_present_display 'K'

set -g __fish_svn_prompt_token_other_char 'O'
set -g __fish_svn_prompt_token_other_color --underline purple
set -g __fish_svn_prompt_token_other_display 'O'

set -g __fish_svn_prompt_token_stolen_char 'T'
set -g __fish_svn_prompt_token_stolen_color --bold purple
set -g __fish_svn_prompt_token_stolen_display 'T'

set -g __fish_svn_prompt_token_broken_char 'B'
set -g __fish_svn_prompt_token_broken_color --bold magenta
set -g __fish_svn_prompt_token_broken_display 'B'

function parse_svn_status --argument status_string
	set flags added conflicted deleted ignored modified replaced unversioned_external unversioned missing locked scheduled switched token_present token_other token_stolen token_broken
	for index in (seq (count $flags))
		set flag_name __fish_svn_prompt_{$flags[$index]}_char
		set has_flag (echo $status_string | grep -c $$flag_name)
		if [ $has_flag -eq 1 ];
			set flag_display __fish_svn_prompt_{$flags[$index]}_display
			set flag_color __fish_svn_prompt_{$flags[$index]}_color
			printf '%s%s%s' (set_color $$flag_color) $$flag_display (set_color normal)
		end
	end
end

function source_control_prompt
	if [ $HAS_GIT = true ];
		git rev-parse 2> /dev/null
		if [ $status -eq 0 ];
			printf '%s' (__fish_git_prompt)
		end
	end
	
	if [ $HAS_SVN = true ];
		set svn_info (svn info $pwd 2> /dev/null)
		if [ $status -eq 0 ];
			set svn_revision (svn info $pwd | grep "Last Changed Rev: " | sed -e "s=Last Changed Rev: ==" -e "s=\n==g")
			printf ' (%s%s%s'  (set_color $__fish_git_prompt_color_branch) $svn_revision (set_color normal) 
			set svn_status_lines (svn stat | awk '{print substr($0,0,7)}')
			set found_first_column false
			set previous_col_empty false
			for col in (seq 6)
				set current_status_line (echo "$svn_status_lines" | awk -FS="" -v col=$col '{print $col}' | sort | uniq)
				set svn_status (echo -n "$current_status_line" | sed -e "s=[\n| ]==g")
				
				if [ "$svn_status" != "" ];
					set found_first_column true
				end
				
				if [ $found_first_column = true ];
					if [ "$svn_status" = "" ];
						set previous_col_empty true
					end
					
					if [ $previous_col_empty = true ];
						printf '|'
					end
					
					if [ "$svn_status" != "" ];
						printf '%s' (parse_svn_status $svn_status)
					end
				else
					printf '|'
				end
			end
			printf ')'
		end
	end
end

function prompt_current_working_dir
	set path_prefix ""
	set working_path (pwd | sed -e "s=^$HOME=~=")
	set total_length "38"
	if [ $HAS_WC = true ];
		#setting up current working dir for path truncation
		set user_length (echo -n $USER | wc -c)
		set host_length (echo -n $__fish_prompt_hostname | wc -c)
		set working_path_length (echo -n $working_path | wc -c)
		set total_length (math "40-$user_length-1-$host_length-1")
		if [ $working_path_length -gt $total_length ];
			set total_length (math "$total_length-3")
			set path_prefix "..."
		end
	end
	set working_path_sub (echo -n $working_path | tail -c $total_length)
	echo -n $path_prefix$working_path_sub
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
		set -g __fish_prompt_hostname (hostname | sed 's=\.local$==')
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
	printf '%s' (source_control_prompt)
	printf '%s' $__fish_prompt_normal
	printf ' $ '	
end