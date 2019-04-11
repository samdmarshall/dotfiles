
function tree --wraps=tree
  set --local display_paths (opt_drop $argv)
  set --local defaults --noreport -F -U 
  set --local ignore_patterns -I .git -I .hg -I .svn
  if test (count $display_paths) -eq 0
    set display_paths (pwd)
  end

  eval command tree $defaults $ignore_patterns -- $display_paths
end
