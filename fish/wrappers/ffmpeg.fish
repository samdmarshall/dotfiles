switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function video2gif --wraps=ffmpeg
            command ffmpeg -vf scale=640:-1 -gifflags +transdiff ~/Desktop/out.gif -i
        end
end
