function rename --argument-names old_name new_name # --argument oldname --argument newname	
  for file in (command ls -a | command grep "$old_name")
    command mv $file (command echo {$file} | command sed "s=$old_name=$new_name=g")
  end
end
