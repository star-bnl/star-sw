#! /bin/tcsh -f
set list = `grep -l 'Break' *.log`
foreach f ( $list ) 
    set n = `basename $f .log`
    echo "remove $n.log and $n.root"
    rm $n.log $n.root
end
# e o d 
