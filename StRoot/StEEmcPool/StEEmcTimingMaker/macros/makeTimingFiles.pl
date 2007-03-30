#!/opt/star/sl305_gcc323/bin/perl -w

my $nevents = 30000;

my @runs = ( 8081046,     8081047,     8081048,     8081049,     8081050,
	     8081051,     8081052,     8081053,     8081056,     8081057,
	     8081058,     8081059,     8081060,     8081061,     8081062
	     );
#	     8081063

@tower_delays = ( 12.0,	  0.0,	  5.0,	  10.0,	  15.0,   
		  25.0,	  35.0,	  45.0,	  55.0,   20.0,
		  30.0,	  40.0,	  60.0,	  50.0,   -10.0 );

@mapmt_delays = ( 55.,	  0.,	  10.,	  20.,	  30.,
		  50.,	  70.,	  60.,	  40.,	  55.,
		  45.,	  35.,	  55.,	  25.,	  55. );
		  
print "nruns=$#runs ndelays=$#tower_delays $#mapmt_delays\n";

$i = 0;
foreach $run ( @runs ) {

#    print "$run $tower_delays[$i] $mapmt_delays[$i]\n";

    $cmd = "bsub -q star_cas_dd -o $run.log -J $run root4star -q -b runEEmcTiming.C\\\($nevents,\\\"$run.list\\\",\\\"$run.root\\\",$tower_delays[$i],$mapmt_delays[$i]\\\)\n";
    open RUNIT, "$cmd|";
    while ( <RUNIT> ) { print; }

    $i++;

}
