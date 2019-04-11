
set --unexport --local rune_path     (command --search rune)
set --unexport --local grimoire_path (command --search grimoire)

set --unexport --local command_v_node_path (command -v node)
if test -n "$command_v_node_path" -a -x "$command_v_node_path"
  set --unexport --local command_v_nodejs_path (command -v nodejs)
  if test -z "$command_v_nodejs_path"
    alias nodejs="node"
  end
end

if test -z $rune_path
	echo "cannot find `rune` in PATH!"
	exit 1
end

if test -z $grimoire_path
  echo "cannot find `grimoire` in PATH!"
  exit 1
end

if test \( ! -x $rune_path \) -o \( ! -x  $grimoire_path \)
  echo "grimoire and rune not found!! exiting early!!"
  exit 1
end

for app in (grimoire --list-enabled)
  if test -n (command --search $app)
    alias $app "grimoire $app"
  end
end

set --local --unexport unsupported_bash_scripts "$HOME/.nvm/nvm.sh"
for script in $unsupported_bash_scripts
  if test -e $script
    set --local --unexport script_name (basename --suffix=.sh $script)
    alias $script_name "bass source $script --no-use ';' $script_name " 
  end
end


abbr --add --global remove "rm --recursive --dir"
abbr --add --global video2gif "ffmpeg -vf scale=640:-1 -gifflags +transdiff -i <input> <output.gif>" 
abbr --add --global certinfo "openssl x509 -inform DER -text -in"
