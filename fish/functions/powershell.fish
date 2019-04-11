if string match --quiet '*+WSL' "$PLATFORM_NAME" or string match --quiet 'Windows*' "$PLATFORM_NAME"
  function powershell
    set --local --unexport wsudo_path      "/mnt/c/ProgramData/chocolatey/bin/wsudo.exe"
    set --local --unexport powershell_path "/mnt/c/Windows/SysWOW64/powershell.exe"
    command $wsudo_path $powershell_path $argv  &; disown
  end
  alias psh="powershell"
end
