function fuzzyfind
  set -l result (find . -type d -not \( -path "*/.git" -or -path "*/.git/*" \) | fzf)
  if test -n $result
    cd $result
  end
end
