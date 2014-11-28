#!/usr/bin/tclsh
puts "echo ' Task3: loop over all RUN is the wrk-directory and execute LCP finder on every'"

set my(cut) "0" ; set my(outPath) "default-H/"
#set my(cut) "1" ; set my(outPath) "nPrim5_20-H/"
#set my(cut) "2" ;  set my(outPath) "zVert50-H/"
#set my(cut) "3" ;  set my(outPath) "pT1_3-H/"
#set my(cut) "4" ; set my(outPath) "posCharge-H/"

set my(wrkDir) "/star/data04/sim/balewski/LcpRun2/maxEta1.0/"
set my(maxEve)   -2000 ;# max number of events to be processed
set my(totLcp) 0

puts "echo 'outPath=$my(outPath)'"

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
proc checkPro {runLog} {
    global my
    #puts "check for $runLog"
    catch  { exec  grep # $runLog }  out
	set outL [split $out "\n"]
	#puts $outL
	set keys "run sort Integral "
	set sum ""
	foreach x $keys {
	    set y "[lindex $outL [lsearch -regexp $outL $x]]" 
	    set sum "$sum $y"
	}
	
	#puts $sum
	set sumL [split $sum " "]
	#puts $sumL
	set run "[lindex $sumL 2]"
	set nEve "[lindex $sumL 6]"
	set nLcp "[lindex $sumL 8]"
	set eff -1
	if { [string length $nEve]>0 && [string length $nLcp]>0 } {
	    set eff [format %.2f [expr $nLcp/$nEve]]
	    set my(totLcp) [expr $my(totLcp)+ $nLcp]
	} else {
	    set nLcp -99
	}
	puts  "echo '$run  [format %6d $nEve] [format %6.0f $nLcp]   $eff  $my(totLcp)'"
	
		
}

catch  { exec  "ls" "$my(wrkDir)/tree" }   treeL
catch  { exec  "ls" "$my(wrkDir)/$my(outPath)" }   histoL
#puts $dirList

set nd 0
set nd1 0
set nd2 0
foreach x $treeL {
    set y [string first  "tree.root"  $x] 
    if { $y < 0 } continue
    incr nd
    set run [string range $x 0 7]
    set z [lsearch -exact $histoL $run.hist.root ]
    set proLcpLog "$my(wrkDir)/$my(outPath)/log/pro$run.log" 
    if { $z>=0 } { checkPro $proLcpLog; incr nd1; continue; }
    incr nd2
    puts "$nd $run $z"
   # puts "\nroot -b -q 'proLcpTree.C($my(cut),\"$run\",$my(maxEve), \"$my(wrkDir)\", \"$my(outPath)\")' >& $proLcpLog"
}
puts "echo 'wrkDir=   $my(wrkDir)$my(outPath)'"
puts "echo 'total tree=$nd  toDo=$nd2 done=$nd1  done_nLcp=$my(totLcp)'"


