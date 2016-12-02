function edit
    set -l fzf_exec_time (date "+%s")
    set -l fzf_tmp_path $TMPDIR/fzf-file-$fzf_exec_time.output
    fzf >$fzf_tmp_path
    set -l fzf_selected_path (cat $fzf_tmp_path)
    rm $fzf_tmp_path
    micro $fzf_selected_path
end
