set --local --unexport platform


set --local --unexport kernel_name (string lower (command uname -s))

switch $kernel_name
  case "darwin"
    set --append platform "Darwin"
  case "linux"
    set --append platform "Linux"
end


set --local --unexport kernel_release (string lower (command uname -r))

switch $kernel_release
  case "*microsoft*"
    set --append platform "WSL"
end


set --export --global PLATFORM_NAME (string join '+' $platform)
set --export --global PLATFORM_ARCH (command uname -p)

