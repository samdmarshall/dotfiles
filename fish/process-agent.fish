#!/usr/bin/env fish

function setup-process-agent
  set process "$argv"
  set processPID (pgrep -fx $process)
  function process-agent-$processPID --on-process-exit $processPID --inherit-variable processPID --inherit-variable process
    functions --erase "process-agent-$processPID"
    eval $process &; disown
    setup-process-agent $process
  end
end
