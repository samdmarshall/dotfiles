function la --wraps=ls

  set --local paths
  set --local additional_flags

  for arg in $argv
    if test -e $arg -o -d $arg
      set --append paths $arg
    else
      set --append additional_flags $arg
    end
  end

  set --local ignore_patterns
  if not contains -- -a $additional_flags
    set --append ignore_patterns \
      --ignore-backups           \
      --ignore="desktop.ini"     \
      --ignore="ntuser.ini"      \
      --ignore="NTUSER.DAT*"     \
      --ignore="ntuser.dat*"     \
      --ignore="\#*\#"           \
      --ignore="*~"
  end

  set --local natural_number_ordering "-v"
  set --local no_owner_display "-g"

  set --local flags          \
    --format=verbose         \
    --almost-all             \
    --human-readable         \
    --no-group               \
    --classify               \
    --color=always           \
    --time=access            \
    --author                 \
    --sort=none              \
    --quoting-style=literal  \
    $no_owner_display        \
    $natural_number_ordering \
    $ignore_patterns

  command ls $flags $additional_flags -U $paths
end
