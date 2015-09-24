
# set __fish_svn_prompt_char_conflicted 'C'
# set __fish_svn_prompt_color_conflicted (set_color --underline magenta)
#
# set -g __fish_svn_prompt__char ''
# set -g __fish_svn_prompt__color (set_color )
# set -g __fish_svn_prompt__display ''
#
# set __fish_svn_prompt_char_deleted 'D'
# set __fish_svn_prompt_color_deleted (set_color red)
#
# set __fish_svn_prompt_char_ignored 'I'
# set __fish_svn_prompt_color_ignored (set_color --bold yellow)
#
# set __fish_svn_prompt_char_modified 'M'
# set __fish_svn_prompt_color_modified (set_color blue)
#
# set __fish_svn_prompt_char_replaced 'R'
# set __fish_svn_prompt_color_replaced (set_color cyan)
#
# set __fish_svn_prompt_char_unversioned_external 'X'
# set __fish_svn_prompt_color_unversioned_external (set_color --underline cyan)
#
# set __fish_svn_prompt_char_unversioned '?'
# set __fish_svn_prompt_color_unversioned (set_color purple)
#
# set __fish_svn_prompt_char_missing '!'
# set __fish_svn_prompt_color_missing
#
# set __fish_svn_prompt_char_versioned_obstructed '~'
# set __fish_svn_prompt_color_versioned_obstructed
#
# set __fish_svn_prompt_char_locked 'L'
# set __fish_svn_prompt_color_locked
#
# set __fish_svn_prompt_char_history '+'
# set __fish_svn_prompt_color_history
#
# set __fish_svn_prompt_char_switched 'S'
# set __fish_svn_prompt_color_switched
#
# set __fish_svn_prompt_char_token_present 'K'
# set __fish_svn_prompt_color_token_present
#
# set __fish_svn_prompt_char_token_other 'O'
# set __fish_svn_prompt_color_token_other
#
# set __fish_svn_prompt_char_token_stolen 'T'
# set __fish_svn_prompt_color_token_stolen
#
# set __fish_svn_prompt_char_token_broken 'B'
# set __fish_svn_prompt_color_token_broken

function checkdir

set status_string 'M?!';
set flags added conflicted deleted modified;
for index in (seq (count $flags))
echo $index
	set flag_name __fish_svn_prompt_{$flags[$index]}_char
	set has_flag (echo $status_string | grep -c $$flag_name)
	if [ $has_flag -eq 1 ];
		set flag_display __fish_svn_prompt_{$flags[$index]}_display
		set flag_color __fish_svn_prompt_{$flags[$index]}_color
		printf '%s%s%s' $$flag_color $$flag_display (set_color normal)
	end
end
	# set svn_status_lines (svn stat | awk '{print substr($0,0,7)}')
# 	for col in (seq 6)
# 		set current_status_line (echo "$svn_status_lines" | awk -FS="" -v col=$col '{print $col}' | sort | uniq)
# 		set svn_status (echo -n "$current_status_line" | sed -e "s=[\n| ]==g")
# 		if [ "$svn_status" != "" ];
# 			set flags added modified
# 			for i in (seq (count $$flags))
# 			        echo $$flags[1]
# 			end
# 		end
# 	end
end
