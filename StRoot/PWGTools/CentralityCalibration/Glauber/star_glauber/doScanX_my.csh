#!/bin/csh

starver SL19b

set nppbin  = "1"
set nppmin  = $1
set nppmax  = $1

set kbin    = "1"
set kmin    = $2
set kmax    = $2

set xbin    = "1"
set xmin    = $3
set xmax    = $3

set efficiency = $4

set nevents =  "1000000" # 1M


#real:the data file with the corrected Refmult distribution in the real data analysis
#set real    = "run18.27Gev.MB.refMultCorrWithWeight.histo.root"

#set real    = "run18_27Gev_MB_refMultCorr.root"
set real    = "Ru_6120files_PileupSubtracted.root"
#set real    = "run18_27Gev_MB_refMultCorr_Weighted.root"
set mc      = "ncoll_npart.root"
set multCut = "50" # 100 for 200 GeV, only fit the mid to tail range of corrected Refmult distribution

root4star -b <<EOF
  .L doNbdFitMaker.C
  scan($nevents, "$real", "$mc", $multCut, $nppbin, $nppmin, $nppmax, $kbin, $kmin, $kmax, $xbin, $xmin, $xmax, $efficiency, 1.00, kFALSE);
  .q
EOF

