#!/usr/bin/tclsh
puts " Task6:  build web page out of log-file "

set my(wrkDir)  final/

#set my(wrkDir) $my(wrkDir)/default-H/
#set my(wrkDir) $my(wrkDir)/maxEta1.4-H/
#set my(wrkDir) $my(wrkDir)/nPrim5_20-H/
set my(wrkDir) $my(wrkDir)/zVert50-H/
#set my(wrkDir) $my(wrkDir)/pT1_3-H/
#set my(wrkDir) $my(wrkDir)/swapPatt-H/
#set my(wrkDir) $my(wrkDir)/shiftPatt-H/
#set my(wrkDir) $my(wrkDir)/negCharge-H/

set my(sigThr)  2.
set my(chi2L)  0.5
set my(chi2H)  1.5

set log ../$my(wrkDir)/logA
puts "check for $log"
set err [  catch  { exec  grep \#\# $log   }  out]
if { $err >0 } { puts "missing $log"; return 0}
set outL [split $out "\n"]
#puts "err=$err $outL"


proc extractCut {cut outL} {
    global my
    set coreFile "$my(wrkDir)asy$cut"
    puts "<tr><td>&nbsp; <b>$cut </b> <br>&nbsp; <a href=\"$coreFile.ps\"> (ps)</a>"
    puts "     <br> &nbsp;<a href=\"$coreFile.gif\"> (gif)</a>"
    set iobs 0
    set obsL "a1 b1 c0 c2 a0 a2 b0 b2 c1"
    foreach line $outL {
	set y [string first  $cut  $line] 
	if { $y < 0 } continue	
	# line has good cut
	#puts $y$line
	set itemL [split $line ","]
	#puts $itemL

	set key [lindex $obsL $iobs]
        set obs [lindex $itemL 1]
	set y [string first  $key  $obs]
	#puts "key=$key obs=$obs y=$y"
	incr iobs
	if { $y < 0 } { puts "<td> "; continue;}	
	# correct observable was found

	set val  [lindex $itemL 3]
        set err  [lindex $itemL 4]
	set chi2 [lindex $itemL 5]
       
	set xSig [expr (abs($val)/$err) > $my(sigThr) ]
	set xChi [expr $chi2<$my(chi2L) || $chi2>$my(chi2H) ]

	puts  -nonewline "  <td>" 
	if { $xSig } { puts "<font  COLOR=F00000>" }
	puts  -nonewline " <b>[format %.2E $val ]</b><br>[format %.2E $err] "
	if { $xSig } { puts  "</font>"}

	if { $xChi } { puts "<font  COLOR=00F000>" }
	puts -nonewline  "<br>&nbsp;&nbsp; <i>[format %.2f $chi2 ] </i> " 
	if { $xChi } { puts  "</font>"}
	puts ""
	#exit
    }

}

set cutL "All"
set cutL "All Pt1    Pt2    Pt3    Pt4    Pt5       PtL    PtM    PtH    EtaBB    EtaBc    EtaFc    EtaFF    Qpos    Qneg"

foreach cut $cutL {
 extractCut $cut $outL
}