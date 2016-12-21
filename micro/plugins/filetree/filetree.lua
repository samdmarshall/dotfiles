VERSION = "1.2.6"

treeView = nil
cwd = DirectoryName(".")
driveLetter = "C:\\"
isWin = (OS == "windows")

function OpenTree()
    CurView():VSplitIndex(NewBuffer("", ""), 0)
    treeView = CurView()
    treeView.Width = 30
    treeView.LockWidth = true
    SetLocalOption("ruler", "false", treeView)
    SetLocalOption("softwrap", "true", treeView)
    SetLocalOption("autosave", "false", treeView)
    SetLocalOption("statusline", "false", treeView)    
    tabs[curTab+1]:Resize()
    RefreshTree()
end

function RefreshTree()
    treeView.Buf:remove(treeView.Buf:Start(), treeView.Buf:End())
    treeView.Buf:Insert(Loc(0,0), table.concat(scandir(cwd), "\n"))
end

-- When user press enter
function preInsertNewline(view)
    if view == treeView then
        local selected = view.Buf:Line(view.Cursor.Loc.Y)
        if view.Cursor.Loc.Y == 0 then
            return false -- topmost line is cwd, so disallowing selecting it
        elseif isDir(selected) then
            cwd = JoinPaths(cwd, selected)
            RefreshTree()
        else
            local filename = JoinPaths(cwd, selected)
            if isWin then filename = driveLetter .. filename end
            CurView():VSplitIndex(NewBuffer("", filename), 1)
            CurView():ReOpen()
            tabs[curTab+1]:Resize()
        end
        return false
    end
    return true
end

-- don't use build-in view.Cursor:SelectLine() as it will copy to clipboard
function SelectLine(v)
    local y = v.Cursor.Loc.Y
    v.Cursor.CurSelection[1] = Loc(0, y)
    v.Cursor.CurSelection[2] = Loc(v.Width, y)
end

-- disallow selecting topmost line in treeview:
function preCursorUp(view) 
    if view == treeView then
        if view.Cursor.Loc.Y == 1 then
            return false
end end end

-- 'beautiful' file selection:
function onCursorDown(view) if view == treeView then SelectLine(view) end end
function onCursorUp(view)   if view == treeView then SelectLine(view) end end


--[[ allows for deleting files
function preDelete(view)
    if view == treeView then
        messenger:YesNoPrompt("Do you want to delete ...?")
        return false
    end
    return true
end
]]--

-- don't prompt to save tree view
function preQuit(view)
    if view == treeView then
        view.Buf.IsModified = false
    end
    return true
end

function scandir(directory)
    local i, t, popen = 3, {}, io.popen
    local pfile
    t[1] = (isWin and driveLetter or "") .. cwd
    t[2] = ".."
    if isWin then
        pfile = popen('dir /a /b "'..directory..'"')
    else
        pfile = popen('ls -Ap "'..directory..'"')
    end
    for filename in pfile:lines() do
        t[i] = filename
        i = i + 1
    end
    pfile:close()
    return t
end

function isDir(path)
    local status = false
    local pfile
    if isWin then
        pfile = io.popen('IF EXIST ' .. driveLetter .. JoinPaths(cwd, path) .. '/* (ECHO d) ELSE (ECHO -)')
    else
        pfile = io.popen('ls -adl "' .. JoinPaths(cwd, path) .. '"')
    end
    if pfile:read(1) == "d" then
        status = true
    end
    pfile:close()
    return status
end

MakeCommand("tree", "filetree.OpenTree", 0)
