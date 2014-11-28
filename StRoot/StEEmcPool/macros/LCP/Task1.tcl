#!/usr/bin/tclsh
puts " Task1: select polarized RHIC fills &\n localize muDst using the file Catalog"
puts " output: outVer1/master.lis <-- FILL:{run-list}\n     outVer1/run/*.lis <-- muDst for every run"


source fill2run.tcl
source getFill.tcl

set my(runSel)   ../JanRunSel/Run2Selection.dat
set my(fillSel)  ../OsamuPol/polVsFill.dat
set my(perlExec) job1.perl
set my(wrkDir)   ./wrk
set my(lowPol)  0.06 ;# beam polarization limit

global my;# my setup
global sum ;# summary

#:::::::::::::::::::::::::::::::::
proc findMuDst {run} {
    global my  
    global sum 
    #    puts "perlExec=$my(perlExec)"
    #    puts "findMuDst($run)"
    set runX [string trim $run "R"]
    #    puts "output=$run"

    set perlJob "/afs/rhic.bnl.gov/star/packages/scripts/get_file_list.pl -keys 'path,filename' -cond 'filetype=daq_reco_MuDst,runnumber=$runX,production=P02ge,storage=local,sanity=1' -delim '/' -limit 1000 "; # was storage=NSF

    set muDstList $my(wrkDir)/muDst$run.lis
    set fd [open $my(perlExec) "w"]
    puts $fd $perlJob
    close $fd
    catch { exec ./$my(perlExec) >$muDstList }
#    catch { exec  '$perlJob' } out
#    puts "out=$out"
    return $muDstList
}

#set fill [ lindex $argv 0]


   
#::::::::::::::::::::  M A I N   ::::::::::::::::::::    

set fillL [ getFill $my(fillSel)]
#set fill "F2111" ; #tmp

set totEve 0
set totNrun 0
set totFill 0

foreach fill $fillL {
    

    puts "\nFILL=$fill  pol: B=$sum($fill-pB)  Y=$sum($fill-pY)"
    if { $sum($fill-pB) < $my(lowPol) && $sum($fill-pY) < $my(lowPol)} {continue}
    set runL [fill2run $fill $my(runSel)]
#    set runL "R2363036"; #tmp
    
    set nRinF 0
    foreach run $runL {
	
	#    puts "run=$run"
	set fileSet [ findMuDst $run]
#	puts "fileSet=$fileSet"
	catch { exec  nl $fileSet | tail -n 1 } out
#	puts "out=$out"

	set nMu [lindex $out 0]	
	if { [string length $nMu] <=0} {set nMu 0}
	set muPath [lindex $out 1]
	set flag " "
	if { $nMu<$sum($run-nSeq) } { set flag "*" }
	if { $nMu>$sum($run-nSeq) } { set flag "!" }
	puts "$flag $run ([format %3d $nMu ] of [format %3d $sum($run-nSeq)]) [format %6d $sum($run-nTrig)] $muPath"
	set w [expr $nMu/ $sum($run-nSeq). ]
	if { $nMu>$sum($run-nSeq) || $sum($run-nSeq)<0 } {set w 1.}
	set totEve [expr $sum($run-nTrig)*$w  +$totEve]
	incr totNrun
	if { $nMu> 0 } {incr nRinF }

#	    break
#	exit
    }
    if { $nRinF >0 } {    incr totFill }
    puts "tot MinB eve=[format %.0f $totEve],  totNrun=$totNrun totFill=$totFill"
}









