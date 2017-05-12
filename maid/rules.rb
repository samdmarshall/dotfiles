Maid.rules do

  rule 'Clean up after diffoscope' do
    dir("#{ENV['TMPDIR']}/*").each do |path|
      file_name = File.basename(path)
      remove(path) if (file_name.start_with?("diffoscope-tmp.") and 1.hour.since?(accessed_at(path)))
    end
  end
  
end
