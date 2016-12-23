#complete --command otool --short-option s --arguments "(command otool -l  | command grep 'segname' | command sed -e 's=^.*segname ==g' | command uniq)"
