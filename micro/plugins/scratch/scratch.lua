VERSION = "1.0.0"

MakeCommand("scratch", "scratch.new_document", 0)

function new_document()
    local scratch_dir = os.getenv("TMPDIR")
    local random_name = string_random(16, 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
    local scratch_path = JoinPaths(scratch_dir, random_name)
    if CurView().Buf:GetName() == "No name" then
        CurView():Open(scratch_path)
    else
        HandleCommand("tab")
        CurView():Open(scratch_path)
    end
end


local Chars = {}
for Loop = 0, 255 do
   Chars[Loop+1] = string.char(Loop)
end
local String = table.concat(Chars)

local Built = {['.'] = Chars}

local AddLookup = function(CharSet)
   local Substitute = string.gsub(String, '[^'..CharSet..']', '')
   local Lookup = {}
   for Loop = 1, string.len(Substitute) do
       Lookup[Loop] = string.sub(Substitute, Loop, Loop)
   end
   Built[CharSet] = Lookup

   return Lookup
end

function string_random(Length, CharSet)

   local _CharSet = CharSet or '.'

   if _CharSet == '' then
      return ''
   else
      local Result = {}
      local Lookup = Built[_CharSet] or AddLookup(_CharSet)
      local Range = #Lookup

      for Loop = 1,Length do
         Result[Loop] = Lookup[math.random(1, Range)]
      end

      return table.concat(Result)
   end
end
