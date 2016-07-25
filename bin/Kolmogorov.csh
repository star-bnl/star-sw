#! /bin/tcsh -f
set list = `ls  y*/*.hist.root`
set tag = "";#.new"
set top = `pwd`
foreach l ($list) 
#    echo $l
    cd $top
    set d = `dirname $l`;# echo $d
    set b = `basename $l hist.root`;# echo $b
    cd $d
    set r1 = ${b}"hist.root"
    echo $r1
    if (-r $r1) then
	set ll = `readlink ${b}log.ref${tag}`
	set dd = `dirname $ll`
	set bb = `basename $l log`
	set r2 = ${dd}"/"${bb}
#	set r2 = ../../DevDevComparision/${d}/${b}hist.root
#	set r2 = ../../root4star/${d}/${b}hist.root
	echo $r2
	if (-r $r2) then
	    echo "Compare $r1 and $r2"
	    set log = "${b}Kolmogorov${tag}"
	    echo "cd $d; root.exe -q  $r1 $r2 Kolmogorov.C | tee  $log; cd ..;" 
#	    root.exe -q $r1 $r2 Kolmogorov.C | tee  $log
	endif
    endif
end
#e o d
