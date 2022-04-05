#!/usr/bin/perl
$minjobs=0;
$speedup=100;
$wait=300;

$numjobs=$minjobs;
while (1){
    $status = `condor_q $ENV{'USER'} | tail -3 | head -1`;
    print $status;
    if ($status ne "") {
        $numjobs = `echo "$status" | cut -d ' ' -f 4`;
#	print $numjobs;
        if ($numjobs<=$speedup) {$wait=60;}
        if ($numjobs<=$minjobs) {exit;}
    }else{
        print "Failed to get information, back to sleep.\n";
    }
    sleep $wait;     
}
