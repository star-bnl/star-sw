#! /bin/tcsh -f
if (! -d Empty) mkdir Empty
foreach f ( `grep -l 'Total events processed :0 and' *B.log` )
 set b = `basename ${f} B.log`; mv ${b}* Empty/
end
