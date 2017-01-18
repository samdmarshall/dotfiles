VERSION = "1.0.3"

ft = {}

ft["c"] = "// %s"
ft["go"] = "// %s"
ft["python"] = "# %s"
ft["html"] = "<!-- %s -->"
ft["java"] = "// %s"
ft["perl"] = "# %s"
ft["rust"] = "// %s"
ft["shell"] = "# %s"
ft["lua"] = "-- %s"
ft["javascript"] = "// %s"
ft["ruby"] = "# %s"
ft["d"] = "// %s"
ft["swift"] = "// %s"

function onViewOpen(v)
    if v.Buf.Settings["commenttype"] == nil then
        if ft[v.Buf.Settings["filetype"]] ~= nil then
            v.Buf.Settings["commenttype"] = ft[v.Buf.Settings["filetype"]]
        else
            v.Buf.Settings["commenttype"] = "/* %s */"
        end
    end
end

function commentLine(lineN)
    local v = CurView()
    local line = v.Buf:Line(lineN)
    local commentType = v.Buf.Settings["commenttype"]
    local commentRegex = "^%s*" .. commentType:gsub("%*", "%*"):gsub("%-", "%-"):gsub("%.", "%."):gsub("%+", "%+"):gsub("%[", "%["):gsub("%%s", "(.*)")
    if string.match(line, commentRegex) then
        uncommentedLine = string.match(line, commentRegex)
        v.Buf:Replace(Loc(0, lineN), Loc(#line, lineN), GetLeadingWhitespace(line) .. uncommentedLine)
    else
        local commentedLine = commentType:gsub("%%s", trim(line))
        v.Buf:Replace(Loc(0, lineN), Loc(#line, lineN), GetLeadingWhitespace(line) .. commentedLine)
    end
    v.Cursor:Relocate()
    v.Cursor.LastVisualX = v.Cursor:GetVisualX()
end

function commentSelection(startLine, endLine)
    for line = startLine, endLine do
        commentLine(line)
    end
end

function comment()
    local v = CurView()
    if v.Cursor:HasSelection() then
        if v.Cursor.CurSelection[1]:GreaterThan(-v.Cursor.CurSelection[2]) then
            commentSelection(v.Cursor.CurSelection[2].Y, v.Cursor.CurSelection[1].Y)
        else
            commentSelection(v.Cursor.CurSelection[1].Y, v.Cursor.CurSelection[2].Y)
        end
    else
        commentLine(v.Cursor.Y)
    end
end

function trim(s)
    return (s:gsub("^%s*(.-)%s*$", "%1"))
end

function string.starts(String,Start)
    return string.sub(String,1,string.len(Start))==Start
end

MakeCommand("comment", "comment.comment")
BindKey("Alt-/", "comment.comment")

AddRuntimeFile("comment", "help", "help/comment-plugin.md")
