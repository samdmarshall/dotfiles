
function cmd_modify --argument-names input
  function __modify_prompt
    set_color green
    echo -n modify
    set_color normal
    echo -n '> '
  end

  read --command=$input --prompt=__modify_prompt --shell modified

  commandline --current-buffer --replace $modified

end
