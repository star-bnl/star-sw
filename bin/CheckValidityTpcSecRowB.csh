#! /bin/tcsh -f
grep 'found table TpcSecRowB' 2*.log | awk '{print $13}' | sort -u | awk -F\: '{print $2}' | awk -F\/ '{printf("TpcSecRowB.%08i.%06i.root\n",$1,$2)}' | xargs

