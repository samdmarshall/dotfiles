switch $PLATFORM_NAME
  case "Linux+WSL"
    function wsl2win
      env --chdir="$WIN_HOME" PATH="$WIN_PATH:$PATH" $argv
    end
end
