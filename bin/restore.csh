#! /usr/local/bin/tcsh -f
set list="TpcdXCorrection.20010701.120000.C  TpcdXCorrection.20030106.000000.C  TpcdXCorrection.20031120.000000.C"
#TpcAdcCorrection.20010701.120000.C  TpcLengthCorrection.20010701.120000.C  TpcZCorrection.20010701.120000.C \
#TpcAdcCorrection.20030106.000000.C  TpcLengthCorrection.20030106.000000.C  TpcZCorrection.20030106.000000.C \
#TpcAdcCorrection.20031120.000000.C  TpcLengthCorrection.20031120.000000.C  TpcZCorrection.20031120.000000.C"
foreach f ($list) 
    cvs update -r SL04g_1 -p $f > $f
end
# e  o  d
