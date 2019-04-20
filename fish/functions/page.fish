
function setup-page-output-error --argument-names output error arguments
  if test "$output" != "/dev/null"
    eval "kitty @ new-window --no-response --title=Output --keep-focus tail -f $output"
  end
  if test "$error" != "/dev/null"
    eval "kitty @ new-window --no-response --title=Error  --keep-focus tail -f $error"
  end

  eval "$arguments 1> $output 2> $error"
  wait
  read --nchars=1 --prompt="" --local --unexport dummy

  if test "$error" != "/dev/null"
    eval "kitty @ close-window --match title:Error"
  end
  if test "$output" != "/dev/null"
    eval "kitty @ close-window --match title:Output"
  end
end

function page-no-error
  setup-page-output-error (mktemp --suffix=stdout) /dev/null "$argv"
end

function page-no-output
  setup-page-output-error /dev/null (mktemp --suffix=stderr) "$argv"
end

function page
  setup-page-output-error (mktemp --suffix=stdout) (mktemp --suffix=stderr) "$argv"
end
