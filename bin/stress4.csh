#! /bin/tcsh -f
foreach d ( 01 02 03 04 )
 cd $d; stress 10000 >& ${d}_`date +%d%b%y`_${SystemName}_nodeb.log &
end
