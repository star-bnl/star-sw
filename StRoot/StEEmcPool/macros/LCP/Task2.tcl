#!/usr/bin/tclsh
puts " Task2: loop over FILL/RUN, accumulate LCP-tree & some bXing histo"
puts " output: outVer1/tree/R*.root"

set my(fillSel) outVer1/master.lis
set my(bxOffFile) ../JanBXoff/bXoffVsRun.dat
set my(muDstPath) outVer1/run

set my(maxEta)   1.0 ;# max Eta cut 
set my(maxSeq)   5000 ;# max number of file-seq to be chained
set my(maxEve)   20000000 ;# max number of events to be processed
set my(wrkDir)  /star/data04/sim/balewski/LcpRun2/maxEta$my(maxEta)

source masterFill.tcl
global master 

source getBXoff.tcl
global bxOff 

getBXoff $my(bxOffFile)
set fillL [masterFill $my(fillSel)]

#set fillL F2201; #tmp

set nJob 0
set nFail 0

puts "wrkDir=   $my(wrkDir)="
foreach fill $fillL {
#        puts "T2: work on fill=$fill, runs=$master($fill-R)"
    foreach run $master($fill-R) {
	# puts "T2: work on run=$run -->$muDst"
	set sortLcpLog "$my(wrkDir)/log/lcp$run.log"
	set scanBxLog  "$my(wrkDir)/log/bx$run.log"
	
	#........ generate muDst SORT jobs .......
	set off48 0
	if { [info exist bxOff($run-48)] } {  set off48 $bxOff($run-48)}
	
	set muDstList "master_tcl/$my(muDstPath)/muDst$run.lis"
	incr nJob
       
	catch {exec grep bX120: $scanBxLog} out
#	puts "out=$out"
	set outL [split $out]
	set x [lsearch $outL bX120:]
	set off -7
	if { $x >=0 } {
	    set off [lindex  $outL [expr $x +2]]
	    #	    puts "x=$x"	   
	}
#	if { $off==0 } {continue}
	if { $off!=0 } {incr nFail}
	puts "$run off=$off" 

#	  exit	
	
#	puts "\nroot4star -b -q 'rdMuDst2LcpTree.C(\"$muDstList\", $my(maxSeq), \"$my(wrkDir)\",$off48, $my(maxEve),$my(maxEta))' >& $sortLcpLog"
	 
#	puts "root.exe -b -q 'scanBXoff.C(\"$run\",\"$my(wrkDir)\")' >& $scanBxLog"
	
    }

}
puts "wrkDir=   $my(wrkDir)="
puts "tot nJob=$nJob, nFail=$nFail"


proc AAA1 {} {
	set run1 [string trimleft $run R]
	set runx [expr ($run1 -3000000)/1000.]
	puts "$run [format "%7.3f" $runx]  [format "%5d" 5] [format "%5d"  $off] "
}