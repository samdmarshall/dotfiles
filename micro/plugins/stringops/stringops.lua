VERSION = "1.0.1"

function CutToEndOfLine()
    local v = CurView()
    if v.Cursor:HasSelection() ~= true then
        v.Cursor:SetSelectionStart(Loc(v.Cursor.X, v.Cursor.Y))
        v.Cursor:End()
        v.Cursor:SetSelectionEnd(Loc(v.Cursor.X, v.Cursor.Y))
        v.Cursor:DeleteSelection()
        v.Cursor:ResetSelection()
    end
end

function CutToStartOfLine()
    local v = CurView()
    if v.Cursor:HasSelection() ~= true then
        v.Cursor:SetSelectionStart(Loc(v.Cursor.X, v.Cursor.Y))
        v.Cursor:Start()
        v.Cursor:SetSelectionEnd(Loc(v.Cursor.X, v.Cursor.Y))
        v.Cursor:DeleteSelection()
        v.Cursor:ResetSelection()
    end
end

