#! /bin/tcsh -f
set list = `ls y*/*.log`
set tag = ".new"
foreach l ($list) 
    set o = ${l}.ref${tag}
    set b = ${l}.diff${tag}
    if (! -r $b) then
        set oo = ${o}.stripped${tag}
	if (! -r ${oo}) then
	    grep -v 'doPs' ${o} | grep -v 'TMemStat' > ${oo}
	endif
	set ll = ${l}.stripped${tag}
	if (! -r ${ll}) then
	    grep -v 'doPs' ${l} | grep -v 'TMemStat' > ${ll}
	endif
	if (-r $oo && -r $ll) diff -uwb  $oo $ll  > $b
    endif
end
#e o d
