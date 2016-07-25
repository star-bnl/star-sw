#! /usr/local/bin/tcsh -f
set Cpu = \
`grep 'QAInfo:Maker' *B.log | grep  'StTpcRSMaker::TpcRS' | awk '{print $7}' | awk -F\= '{print $2}' | awk -F\( 'BEGIN {n=0 ; j = 0 ; }{j++ ; n += $1 ; } END {print "Total = "n/j/100" sec/event  in "j" files"}'`
echo "Cpu/event: $Cpu"
