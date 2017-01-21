switch (command echo "$FISH_PLATFORM_NAME")
    case 'Darwin' 'darwin'
        function otool --wraps=otool
            if count $argv > /dev/null
                switch $argv[1]
                    case '--segments'
                        for segment_name in (command otool -l $argv[2] | command grep 'segname' | command sed -e 's=^.*segname ==g' | command uniq)
                            echo "$segment_name"
                        end
                    case '--sections'
                        for segment_name in (otool --segments $argv[2])
                            for section_name in (command otool -l $argv[2] | command awk '/^.*sectname/,/^.*segname/' | command awk '{section=$2; getline ; segment=$2; print section" "segment };')
                                echo "$section_name"
                            end
                        end
                    case '*'
                        command otool $argv
                end
            else
                command otool
            end
        end
end
