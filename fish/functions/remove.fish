function remove
  switch $PLATFORM_NAME
    case 'Darwin'
      command rm -r $argv
    case 'Linux*'
      command rm --dir --recursive $argv
  end
end
