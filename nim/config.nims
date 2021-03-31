
mode = ScriptMode.Verbose

switch "listFullPaths", "on"
#switch "NimblePath", "~/.nimble/pkgs"
switch "verbosity", "2"
switch "cc", "clang"
switch "colors", "on"
#switch "index", "on"
#switch "lineTrace", "on"
switch "listCmd"

#switch "genMapping"
#switch "genDeps"
#switch "project"

warning "Deprecated", on
warning "CannotOpenFile", on
warning "ConfigDeprecated", on
#warning "ShadowIdent", on

hint "Dependency", on
hint "Conf", on
hint "Link", on
hint "CC", on
hint "Source", on
hint "XDeclaredButNotUsed", on
hint "Exec", on
hint "LineTooLong", off
hint "GlobalVar", off
hint "Performance", on
hint "Path", on
hint "Processing", on
hint "GCStats", off

#switch "define", "checkAbi:on"

switch "cincludes", "/brew/include"
switch "clibdir", "/brew/lib"
