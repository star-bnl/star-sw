#! /bin/tcsh -f
grep 'row.ScaleFactor' $CVSROOT/StarDb/Calibrations/tpc/Attic/*Time* | awk '{print $1" "$4}' | awk -F. '{print $2" "$3" " $4"."$5}' | awk -F\; '{print $1}' | awk '{print $1" "$2" "$4}' > tpcTimeGain.data

