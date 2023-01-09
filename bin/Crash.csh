#! /bin/tcsh -f
if (! -d Crash) mkdir Crash
foreach f ( `egrep -l '(Memory map|There was a crash)' *B.log` )
 set b = `basename ${f} B.log`; mv ${b}* Crash/
end
