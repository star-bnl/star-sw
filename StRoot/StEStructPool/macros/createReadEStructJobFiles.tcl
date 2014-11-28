set fCsh [open createReadEStructJob_Template.csh]
set cshScript [read $fCsh]
close $fCsh
set fCondor [open createReadEStructJob_Template.condor]
set condorScript [read $fCondor]
close $fCondor

for {set i 0} {$i < 50} {incr i} {
    set fCSh [open schedReadEStructJob_$i.csh "w"]
    puts $fCsh [string map "JOBNUMBER $i" $cshScript]
    close $fCsh
    file attributes schedReadEStructJob_$i.csh -permissions +x
    set fCondor [open schedReadEStructJob_$i.condor "w"]
    puts $fCondor [string map "JOBNUMBER $i" $condorScript]
    close $fCondor
}

for {set i 0} {$i < 50} {incr i} {
    exec condor_submit schedReadEStructJob_$i.condor
}
