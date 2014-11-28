#
#uncomment for total test
set x = ( `find $1 -name '*.root' ` )

#set x = ( $* )
foreach FIL ( $x )
echo FILE $FIL
$STAR/mgr/lsFile.csh $FIL |& grep Error
end
