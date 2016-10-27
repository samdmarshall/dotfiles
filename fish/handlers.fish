function logout_message --on-process-exit %self
	set ESC_SEQ "\x1b[38;5;"
	set COL_01 $ESC_SEQ"160;01m"
	set COL_02 $ESC_SEQ"196;01m"
	set COL_03 $ESC_SEQ"202;01m"
	set COL_04 $ESC_SEQ"208;01m"
	set COL_05 $ESC_SEQ"214;01m"
	set COL_06 $ESC_SEQ"220;01m"
	set COL_07 $ESC_SEQ"226;01m"
	set COL_08 $ESC_SEQ"190;01m"
	set COL_09 $ESC_SEQ"154;01m"
	set COL_10 $ESC_SEQ"118;01m"
	set COL_11 $ESC_SEQ"046;01m"
	set COL_12 $ESC_SEQ"047;01m"
	set COL_13 $ESC_SEQ"048;01m"
	set COL_14 $ESC_SEQ"049;01m"
	set COL_15 $ESC_SEQ"051;01m"
	set COL_16 $ESC_SEQ"039;01m"
	set COL_17 $ESC_SEQ"027;01m"
	set COL_18 $ESC_SEQ"021;01m"
	set COL_19 $ESC_SEQ"021;01m"
	set COL_20 $ESC_SEQ"057;01m"
	set COL_21 $ESC_SEQ"093;01m"
	set RESET "\033[m"

	# Timeless message

	printf "$COL_01  .d8888b.  8888888888 8888888888      Y88b   d88P  .d88888b.  888     888  \n"
	printf "$COL_02 d88P  Y88b 888        888              Y88b d88P  d88P\" \"Y88b 888     888  \n"
	printf "$COL_03  \"Y888b.   8888888    8888888            Y888P    888     888 888     888  \n"
	printf "$COL_04     \"Y88b. 888        888                 888     888     888 888     888  \n"
	printf "$COL_05       \"888 888        888                 888     888     888 888     888  \n"
	printf "$COL_06 Y88b  d88P 888        888                 888     Y88b. .d88P Y88b. .d88P  \n"
	printf "$COL_07  \"Y8888P\"  8888888888 8888888888          888      \"Y88888P\"   \"Y88888P\"  \n"
	printf "$COL_08  .d8888b.  8888888b.     d8888  .d8888b.  8888888888    \n"
	printf "$COL_09 d88P  Y88b 888   Y88b   d88888 d88P  Y88b 888       \n"
	printf "$COL_10  \"Y888b.   888   d88P d88P 888 888        8888888    \n"
	printf "$COL_11     \"Y88b. 8888888P\" d88P  888 888        888   \n"
	printf "$COL_12       \"888 888      d88P   888 888    888 888    \n"
	printf "$COL_13 Y88b  d88P 888     d8888888888 Y88b  d88P 888  \n"
	printf "$COL_14  \"Y8888P\"  888    d88P     888  \"Y8888P\"  8888888888     \n"
	printf "$COL_15  .d8888b.   .d88888b.  888       888 888888b.    .d88888b. Y88b   d88P     \n"
	printf "$COL_16 d88P  Y88b d88P\" \"Y88b 888   o   888 888  \"88b  d88P\" \"Y88b Y88b d88P   \n"
	printf "$COL_17 888        888     888 888 d888b 888 8888888K.  888     888   Y888P    \n"
	printf "$COL_18 888        888     888 888d88888b888 888  \"Y88b 888     888    888    \n"
	printf "$COL_19 888    888 888     888 88888P Y88888 888    888 888     888    888  \n"
	printf "$COL_20 Y88b  d88P Y88b. .d88P 8888P   Y8888 888   d88P Y88b. .d88P    888  \n"
	printf "$COL_21  \"Y8888P\"   \"Y88888P\"  888P     Y888 8888888P\"   \"Y88888P\"     888\n"
	printf "$RESET"
end

function current_path --on-variable PWD
    printf '<'
    printf '%s' $__fish_prompt_host
    printf '%s' $__fish_prompt_hostname
    printf '%s' $__fish_prompt_normal
    printf '> ['
    printf '%s' $__fish_prompt_path
    printf '%s' (prompt_pwd)
    printf '%s' $__fish_prompt_normal
    printf ']'
    printf '\n'
end

