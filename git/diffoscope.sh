#!/usr/bin/env sh

report=`mktemp -d -t diffoscope-tmp`
/usr/local/bin/diffoscope "$2" "$5" --jquery /usr/local/lib/node_modules/jquery/dist/jquery.js --html-dir $report --output-empty
open "$report/index.html"

exit
