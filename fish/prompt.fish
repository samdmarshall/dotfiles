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

function parse_svn_status --argument status_string

	switch $status_string
		case 'A' 'A*' '*A*' '*A'
			printf '%sA%s' (set_color green) (set_color normal)
	end
	
	switch $status_string
		case 'C' 'C*' '*C*' '*C'
			printf '%sC%s' (set_color --underline orange) (set_color normal)
	end
	
	switch $status_string
		case 'D' 'D*' '*D*' '*D'
			printf '%sD%s' (set_color red) (set_color normal)
	end
	
	switch $status_string
		case 'M' 'M*' '*M*' '*M'
			printf '%sM%s' (set_color blue) (set_color normal)
	end
	
	switch $status_string
		case 'R' 'R*' '*R*' '*R'
			printf '%sR%s' (set_color cyan) (set_color normal)
	end
	
	switch $status_string
		case 'X' 'X*' '*X*' '*X'
			printf '%sX%s' (set_color --underline cyan) (set_color normal)
	end
	
	switch $status_string
		case '?' '?*' '*?*' '*?'
			printf '%s?%s' (set_color purple) (set_color normal)
	end
	
	switch $status_string
		case '!' '!*' '*!*' '*!'
			printf '%s!%s' (set_color yellow) (set_color normal)
	end
	
	switch $status_string
		case '~' '~*' '*~*' '*~'
			printf '%s~%s' (set_color orange) (set_color normal)
	end
	
	switch $status_string
		case 'L' 'L*' '*L*' '*L'
			printf '%sL%s' (set_color --bold red) (set_color normal)
	end
	
	switch $status_string
		case '+' '+*' '*+*' '*+'
			printf '%s+%s' (set_color --bold green) (set_color normal)
	end
	
	switch $status_string
		case 'S' 'S*' '*S*' '*S'
			printf '%sS%s' (set_color --bold blue) (set_color normal)
	end
	
	switch $status_string
		case 'K' 'K*' '*K*' '*K'
			printf '%sK%s' (set_color --bold cyan) (set_color normal)
	end
	
	
	switch $status_string
		case 'O' 'O*' '*O*' '*O'
			printf '%sO%s' (set_color --underline purple) (set_color normal)
	end
	
	
	switch $status_string
		case 'T' 'T*' '*T*' '*T'
			printf '%sT%s' (set_color --bold purple) (set_color normal)
	end
	
	
	switch $status_string
		case 'B' 'B*' '*B*' '*B'
			printf '%sK%s' (set_color --bold orange) (set_color normal)
	end
end

function source_control_prompt
	git rev-parse 2> /dev/null
	if [ $status -eq 0 ];
		printf '%s' (__fish_git_prompt)
	end
	
	set svn_info (svn info $pwd 2> /dev/null)
	if [ $status -eq 0 ];
		set svn_revision (svn info $pwd | grep "Last Changed Rev: " | sed -e "s=Last Changed Rev: ==" -e "s=\n==g")
		printf ' (%s%s%s'  (set_color yellow) $svn_revision (set_color normal) 
		set svn_status_lines (svn stat | awk '{print substr($0,0,7)}')
		for col in (seq 6)
			set current_status_line (echo "$svn_status_lines" | awk -FS="" -v col=$col '{print $col}' | sort | uniq)
			set svn_status (echo -n "$current_status_line" | sed -e "s=[\n| ]==g")
			if [ "$svn_status" != "" ];
				printf '|%s' (parse_svn_status $svn_status)
			end
		end
		printf ')'
	end
end

function prompt_current_working_dir
	#setting up current working dir for path truncation
	set user_length (echo -n $USER | wc -c)
	set host_length (echo -n $__fish_prompt_hostname | wc -c)
	set working_path (pwd | sed -e "s=^$HOME=~=")
	set working_path_length (echo -n $working_path | wc -c)
	set total_length (echo "40-$user_length-1-$host_length-1" | bc)
	set path_prefix ""
	if [ $working_path_length -gt $total_length ];
		set total_length (echo "$total_length-3" | bc)
		set path_prefix "..."
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
	printf '%s ' (source_control_prompt)
	printf '%s' $__fish_prompt_normal
	printf '$ '	
end