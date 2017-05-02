# setting up colours
set __fish_prompt_user -o d43582
set __fish_prompt_host b58900
set __fish_prompt_path 299e95
set __fish_prompt_normal normal
set __fish_prompt_hostname (hostname -s)

function display_battery_level
  set -l level (battery-level --default)
  set -l battery_color brgreen
  if test $level -lt 10
    set battery_color brred
  else if test $level -lt 25
    set battery_color red
  else if test $level -lt 50
    set battery_color yellow
  else if test $level -lt 75
    set battery_color green
  end
  if test (battery-level --default --charging) -eq 1
    set battery_color $battery_color --bold
  end
  printf '[%s%s%%%s]' (set_color $battery_color) $level (set_color $__fish_prompt_normal)
end

function fish_right_prompt
  set -l display_elements
  set -l calendar_events (khal list --notstarted --day-format "" today today ^ /dev/null | wc -l)
  if test $calendar_events -gt 0
    set display_elements '#' $display_elements
  end
  if test -e ~/eMail/Inbox/.notmuch/
    set -l unread_emails (notmuch count tag:unread)
    set -l flagged_emails (notmuch count tag:flagged)
    if test $unread_emails -gt 0
      set display_elements $display_elements '@'
    end
    if test $flagged_emails -gt 0
      set display_elements '!' $display_elements
    end
  end
  for element in $display_elements
    printf '%s ' $element
  end
  switch (command echo "$FISH_PLATFORM_NAME")
      case 'Darwin' 'darwin'
        if test (battery-level --list) -gt 0
          display_battery_level
        end
  end
end

function fish_prompt
  printf '(%s%s%s) ' (set_color $__fish_prompt_user) $USER (set_color $__fish_prompt_normal)
end
