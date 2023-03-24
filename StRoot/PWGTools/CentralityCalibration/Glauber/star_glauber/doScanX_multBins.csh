#!/bin/csh

starver SL16d

set nppbin  = "1"
set nppmin  = $1
set nppmax  = $1

set kbin    = $2
set kmin    = $3
set kmax    = $4

set xbin    = $5
set xmin    = $6
set xmax    = $7

set efficiency = $8

set nevents =  "1000000" # 1M
set real    = "run11.MonTrg.refMultCorr.histo.root"
set mc      = "ncoll_npart.root"
set multCut = "100" # 100 for 200 GeV

root4star -b <<EOF
  .L doNbdFitMaker.C
  scan($nevents, "$real", "$mc", $multCut, $nppbin, $nppmin, $nppmax, $kbin, $kmin, $kmax, $xbin, $xmin, $xmax, $efficiency, 1.00, kFALSE);
  .q
EOF
