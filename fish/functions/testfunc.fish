function testfunc
	set svn_info (svn info $pwd 2> /dev/null)
	if [ $status -eq 0 ];
		set svn_revision (svn info $pwd | grep "Last Changed Rev: " | sed -e "s=Last Changed Rev: ==" -e "s=\n==g")
		printf ' (%s%s%s'  (set_color $__fish_git_prompt_color_branch) $svn_revision (set_color normal) 
		set svn_status_lines (svn stat | sed -e 's=^Summary of conflicts.*==' -e 's=^  Text conflicts.*==' -e 's=^  Tree conflicts.*==' -e 's=.*incoming .* upon update.*==' | cut -c 1-7 | tr '\n' ':')
		set found_first_column 0
		set previous_nonempty_column 0
		set status_state ""
		for col in (seq 7)
			set svn_status (echo $svn_status_lines | tr ':' '\n' | cut -c $col | sort | uniq | tr -d '\n' | tr -d ' ')
	
			if [ "$svn_status" != "" ];
				if [ $found_first_column -eq 0 ];
					set found_first_column $col
					set status_state $status_state'|'
				end
			end

			if [ $found_first_column != 0 ];
				if [ "$svn_status" != "" ];
					set previous_nonempty_column $col
				end

				if [ $previous_nonempty_column != $found_first_column ];
					set status_state $status_state'|'
				end

				if [ "$svn_status" != "" ];
					set tags (parse_svn_status $svn_status)
					set status_state $status_state$tags
				end
			else
				set status_state $status_state'|'
			end
		end
		set was_not_empty false
		set status_length (echo $status_state | tr -d '|' | wc -c)
		if [ $status_length -gt 1 ];
			set was_not_empty true
		end
		if [ $was_not_empty = true ];
			printf $status_state
		end
		printf ')'
	end
	
end