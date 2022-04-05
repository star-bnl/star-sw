#!/usr/bin/perl -w

@pid = ("electron","gamma","mu-","pi-","pi0");
#@pid = ("electron","gamma","pi0");
#@pid = ("mu-","pi-");
#@pid = ("electron");
$nev = 1000;
$run1= 100;
$run2= 199;
$outdir1="sim_nomerge";
$outdir2="sim_merge";

my $opt="none";
if($#ARGV==0) {
    $opt=$ARGV[0];
    print "Option = $opt\n";
}else{
    print "Option = none, just creating job file. To submit job, add option submit\n";
}

my $exe="runsimbfc";
my $logdir="log";
my $condordir="condor";
my $cwd=$ENV{'PWD'};

if (! -e "$logdir")   {system("/bin/mkdir $logdir");}
if (! -e "$condordir") {system("/bin/mkdir $condordir");}

my $condor="$condordir/submit_simbfc.txt";
print("Creating $condor\n");
if (-e $condor) {system("/bin/rm $condor");}

open(OUT, "> $condor\n");
print(OUT "Executable   = $exe\n");
print(OUT "Universe     = vanilla\n");
print(OUT "notification = never\n");
print(OUT "getenv       = True\n");
print(OUT "Requirements = CPU_Experiment == \"star\"\n");
print(OUT "+Experiment  = \"star\"\n");
print(OUT "+Job_Type    = \"cas\"\n");
print(OUT "\n");

$njob=0;
foreach $p (@pid) {
    for($r=$run1; $r<=$run2; $r++){
	$log="$cwd/$logdir/bfc.$p.$r.nomg.log";
	print(OUT "Arguments = \"$nev $r $p $outdir1 0\"\n");
	print(OUT "Log    = $log\n");
	print(OUT "Output = $log\n");
	print(OUT "Error  = $log\n");
	print(OUT "Queue\n\n");    
	$njob++;

	$log="$cwd/$logdir/bfc.$p.$r.mg.log";
	print(OUT "Arguments = \"$nev $r $p $outdir2 1\"\n");
	print(OUT "Log    = $log\n");
	print(OUT "Output = $log\n");
	print(OUT "Error  = $log\n");
	print(OUT "Queue\n\n");    
	$njob++;
    }
}
print "$njob jobs created\n";

if($opt eq "submit" ){
    print("Submitting ${condor}\n");
    system("condor_submit ${condor}\n");
    system("running200.pl\n");
    system("mergesim.pl\n");
}
