VERSION = "0.1"

if GetOption("fishfmt") == nil then
    AddOption("fishfmt", true)
end

MakeCommand("fishfmt", "fish.fishfmt", 0)

function onViewOpen(view)
    if view.Buf:FileType() == "fish" then
        SetLocalOption("tabstospaces", "on", view)
    end
end

function onSave(view)
    if CurView().Buf:FileType() == "fish" then
        if GetOption("fishfmt") then
            fishfmt()
        end
    end
end

function fishfmt()
    CurView():Save(false)
    local handle = io.popen("fish_indent -w " .. CurView().Buf.Path)
    local result = handle:read("*a")
    handle:close()
    
    CurView():ReOpen()
end

AddRuntimeFile("fish", "help", "help/fish-plugin.md")
