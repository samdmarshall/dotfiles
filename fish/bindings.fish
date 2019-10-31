
function fish_user_key_bindings
  bind \cD 'commandline ""'
# bind \cE 'kill -SIGHUP (pgrep --newest emacs)'
  bind \cX 'commandline |  cmd_modify'
#  bind \cM 'commandline "echo modify!!"'
end
