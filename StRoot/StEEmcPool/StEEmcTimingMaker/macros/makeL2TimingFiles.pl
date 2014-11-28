#!/opt/star/sl44_gcc346/bin/perl -w

# specify which macro to run
my $macro="runEEmcL2Timing.C"; 

# run numbers
my @runs = ( 8327013,
             8327014,
             8327015,
             8327016,
             8327017,
             8327018,
             8327019,
             8327020,
             8327021,
             8327022,
             8327023,
             8327024,
             8327025,
             8327026,
             8327027 );


# number of events per run
my @nevents = ( 136674,
		20000,
                20000,
                20000,
                20000,
                20000,
                20000,
                20000,
                20000,
                20000,
                20000,
                20000,
                20000,
                20000,
                20000,
                20000,
                20000 );



# tower delays in each run
@tower_delays = ( 5,
                  15,
                  25,
                  35,
                  45,
                  55,
                  65,
                  75,
                  70,
                  60,
                  50,
                  40,
                  30,
                  20,
                  10 );


# mapmt delays in each run
@mapmt_delays = ( 5,
                  15,
                  25,
                  35,
                  45,
                  55,
                  65,
                  75,
                  70,
                  60,
                  50,
                  40,
                  30,
                  20,
                  10 );

		  
# loop over the runs
$i = 0;
foreach $run ( @runs ) 
{

    # command to run
    $cmd = "root4star -q -b $macro\\\($nevents[$i],\\\"$run.list\\\",\\\"$run.root\\\",$tower_delays[$i],$mapmt_delays[$i]\\\)\n";

    # run root4star
    open RUNIT, "$cmd|";
    while ( <RUNIT> ) { print; }

    $i++;

}
