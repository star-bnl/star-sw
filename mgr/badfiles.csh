#
#uncomment for total test
#set x = ( `find $1 -name '*.root' ` )

set x = ( $1/*.root )
foreach FIL ( $x )
$STAR/mgr/lsFile.csh $FIL |& grep Error
end
