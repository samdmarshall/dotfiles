function jenv --argument java_version
  set java_path (command /usr/libexec/java_home -v $java_version)
  if test -n java_path
    set -xg JAVA_HOME "$java_path"
    printf "Updated JAVA_HOME variable!\n"
  end
end
