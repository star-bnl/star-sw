#!/usr/bin/tclsh
#puts " Task5:  loop FILL, sum bXings, add runs, normalize with Lum"

set my(fillSel) outVer1/master.lis

set my(wrkDir)   /star/data04/sim/balewski/LcpRun2/maxEta1.0/

set my(wrkDir) $my(wrkDir)/default-H/
#set my(wrkDir) $my(wrkDir)/maxEta1.4-H/
#set my(wrkDir) $my(wrkDir)/nPrim5_20-H/
#set my(wrkDir) $my(wrkDir)/zVert50-H/
#set my(wrkDir) $my(wrkDir)/pT1_3-H/
#set my(wrkDir) $my(wrkDir)/posCharge-H/
#set my(wrkDir) $my(wrkDir)/swapPatt-H/
#set my(wrkDir) $my(wrkDir)/shiftPatt-H/
#set my(wrkDir) $my(wrkDir)/randPatt-H/



source masterFill.tcl
global master 

puts "echo '"
set fillL [masterFill $my(fillSel)]
puts "'"

#set fillL F2134; #tmp

set nJob 0

#puts "wrkDir=   $my(wrkDir)="

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc checkAddRuns {addLog} {
    global my
    # puts "check for $addLog"
    set err [  catch  { exec  grep #fill $addLog }  out]
	if { $err >0 } { puts "missing $addLog"; return 0}
	set outL [split $out ","]
#	puts "err=$err $outL"

	set fill [lindex [lindex $outL 0] 1]
	puts -nonewline "$fill   "

	set nLcp [lindex [lindex $outL 1] 1]
	puts -nonewline "[format %8.1f $nLcp]   "

	set nAll  [lindex [lindex $outL 2] 1]


	puts -nonewline  "[format %8.1f  $nAll] "

	set eff  [lindex [split "[lindex $outL end]=" "="] 1]

	puts -nonewline "(eff=$eff)    "

	#	puts -nonewline "
	set outL1 [lrange $outL 3 end-1]
	foreach item $outL1 {
	    #puts "$item"
#	    puts -nonewline [lindex $item 0]

	    set nX [lindex $item 1]
	    set eff [format %-7.2g  [expr $nX/$nAll]]
	    puts -nonewline "$eff  "

#	exit
	}
	puts ""

	return $nAll
    }
	       
	      
set sum 0
foreach fill $fillL {
    # if { $fill <"F2189"  } { continue } ;# NO LUM

    #if { $fill> "F2135" && $fill<"F2201" } {continue;}
    #if { $fill> "F2289" } {continue;}
    #    puts "T2: work on fill=$fill, runs=$master($fill-R)"
    incr nJob
    set runL $master($fill-R)
    set addLog "$my(wrkDir)/log/add$fill.log"
    set ratLog "$my(wrkDir)/log/rat$fill.log"
    set sum1 [checkAddRuns $addLog]
    #set sum1 0
    #puts "ss=$sum1"
    #puts "\nroot -b -q 'addRuns.C(\"$fill\", \"$runL\",\"$my(wrkDir)\")' >& $addLog"
    #puts "\nroot -b -q 'doRatio.C(\"$fill\",\"$my(wrkDir)\")' >& $ratLog"
   set sum [expr $sum + $sum1]
}

puts "echo 'dir=$my(wrkDir)'"
puts "echo 'nJob=$nJob totAccepted LCP =$sum '"