#

foreach FIL ( $1/*.root )
$STAR/mgr/lsFile.csh $FIL | grep Error
end
