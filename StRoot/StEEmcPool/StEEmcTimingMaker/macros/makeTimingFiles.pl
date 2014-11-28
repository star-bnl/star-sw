#!/opt/star/sl44_gcc346/bin/perl -w

my $macro="runEEmcTiming.C"; 

my $nevents = 30000;

my @runs = ( 8095096, 8095097, 8095098, 8095099, 8095100, 8095104 ); 

@tower_delays = ( 19., 1., 7., 13., 25., 19. ); 

@mapmt_delays = ( 65., 55., 60., 60., 70., 65. );  
		  
print "nruns=$#runs ndelays=$#tower_delays $#mapmt_delays\n";

$i = 0;
foreach $run ( @runs ) {

#    print "$run $tower_delays[$i] $mapmt_delays[$i]\n";

#$cmd = "bsub -q star_cas_dd -o $run.log -J $run root4star -q -b runEEmcTiming.C\\\($nevents,\\\"$run.list\\\",\\\"$run.root\\\",$tower_delays[$i],$mapmt_delays[$i]\\\)\n";
$cmd = "root4star -q -b $macro\\\($nevents,\\\"$run.list\\\",\\\"$run.root\\\",$tower_delays[$i],$mapmt_delays[$i]\\\)\n";
    open RUNIT, "$cmd|";
    while ( <RUNIT> ) { print; }

    $i++;

}
