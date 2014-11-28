#!/usr/bin/tclsh
puts " skim multiple log files for certain info, bould summary file"


set inpFile "0list1"
set outFile "table.csv"
#===================================
#===================================
proc skimOneLogfile { logName } {

    global eve

    catch { unset eve}

    set xL [split $logName "_"]
    puts $xL
    set job  [lindex $xL 3]
    puts $job
    set xL [split $job "."]
    set job  [lindex $xL 0]
    puts $job
    set eve(job) $job

    ;# .....................N EVE .....
    set err [  catch  { exec grep All $logName  }  out]
    puts "out=$out"
    if { $err!=0 } { ;# unexected result
        puts "err=$err"
        puts "out=$out"
        puts "JB fatal crash1"
        exit
    } else {
	set xL [split $out "I"]
	puts $xL
	set x1  [lindex $xL 2]
	puts $x1
	scan $x1 "%d" nPythia
	puts nPyt=$nPythia
	set sigPythia  [lindex $xL 3]
	puts "nPyt=$nPythia sig=$sigPythia"
	set eve(nPyt) $nPythia
	set eve(sigPyt) $sigPythia
    }

    ;# .....................N GEANT .....
    set err [  catch  { exec grep "NUMBER OF EVENTS PROCESSED" $logName  }  out]
    puts "out=$out"
    if { $err!=0 } { ;# unexected result
        puts "err=$err"
        puts "out=$out"
        puts "JB fatal crash2"
        exit
    } else {
	set xL [split $out "="]
	puts $xL
	set x1  [lindex $xL 1]
	puts $x1
	scan $x1 "%d" nGeant
	puts nGeant=$nGeant
	set eve(nGeant) $nGeant
    }

    ;# ..................... total CPU ...
    set err [  catch  { exec tail -n 1 $logName  }  out]
    puts "out=$out"
    if { $err!=0 } { ;# unexected result
        puts "err=$err"
        puts "out=$out"
        puts "JB fatal crash3"
        exit
    } else {
	scan $out "%f " nSec
	set nDay [format %.3f [expr $nSec/3600./24]]
	puts "nSec=$nSec nDay=$nDay"
	set eve(nSec) $nSec
	set eve(nDay) $nDay
    }

}


#=================================
#           MAIN
#=================================
set fd [open $inpFile "r"]
set fdo [open $outFile "w"]
global eve
set nF 0
set totPytEve 0
set totGeantEve 0
set totCpuDay 0

while {[gets $fd line] >= 1} {
     puts lll=$line
     skimOneLogfile $line

    puts  "Job $eve(job), $eve(nPyt), $eve(sigPyt), $eve(nGeant), $eve(nSec) , $eve(nDay)"
    puts  $fdo "$eve(job), $eve(nPyt), $eve(sigPyt), $eve(nGeant), $eve(nSec) , $eve(nDay)"
    incr totPytEve $eve(nPyt)
    incr totGeantEve $eve(nGeant)
    set totCpuDay [expr $totCpuDay + $eve(nDay)]
#exit
    incr nF
#    if { $nF>10 } break
}

puts "total files=$nF"
puts "total: Pythia/M=[format %.1f [expr $totPytEve/1000000.]] Geant=$totGeantEve cpuDay=$totCpuDay"

exit
set runL [lsort [array names outC]]
puts " list of runs=$runL"

set fdo [open $outFile "w"]

foreach run $runL {
    puts "$run   $outC($run)"
    for {set i 1} {$i <=$outC($run)} {incr i} {
	puts $fdo $outL($run-$i)
    }
}


