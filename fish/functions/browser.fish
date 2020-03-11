function browser
  for url in $argv
    /mnt/c/Windows/System32/cmd.exe /c start microsoft-edge:"$url"
  end
end
