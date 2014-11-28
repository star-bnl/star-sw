#! /usr/local/bin/tcsh -f
set list = `ls -1 *evts* | grep -v Aft-`
foreach file ($list)
  set no = `grep  "NUMBER\ OF\ EVENTS\ PROCESSED\ =" $file | awk '{print $8}'`
  if ( $no != "" ) mv $file ${file}-Aft-Evt-${no}
end
