#!/usr/bin/perl -w

@pid = ("ele","pos");
@e = ("0");
@pt = ("0.6", "0.8", "1.0");
@vz = ("0", "50","-50");
$outdir="out";
$nev = 5000;
$run1= 1;
$run2= 200;
$nogeant=1;

my $opt="none";
if($#ARGV==0) {
    $opt=$ARGV[0];
    print "Option = $opt\n";
}else{
    print "Option = none, just creating job file. To submit job, add option submit\n";
}

my $exe="runsim";
my $condordir="condor";
my $cwd=$ENV{'PWD'};

if (! -e "$outdir") {system("/bin/mkdir $outdir");}
if (! -e "$condordir") {system("/bin/mkdir $condordir");}
my $condor="$condordir/submit_sim.txt";
if (-e $condor) {system("/bin/rm $condor");}
print("Creating $condor\n");
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
  foreach $e (@e) {
      foreach $pt (@pt) {
	  foreach $vz (@vz) {
	      for($r=$run1; $r<=$run2; $r++){
		  $name="$p.pt$pt.vz$vz";
		  $out="$outdir/$name";
		  $logdir="$out/log";
		  $log="$cwd/$logdir/$name.run$r.log";
		  if (! -e "$cwd/$out")     {system("/bin/mkdir $cwd/$out");}
		  if (! -e "$cwd/$logdir")  {system("/bin/mkdir $cwd/$logdir");}
		  
		  print(OUT "Arguments = \"$nev $r $p $e $pt $vz $out $nogeant\"\n");
		  print(OUT "Log    = $log\n");
		  print(OUT "Output = $log\n");
		  print(OUT "Error  = $log\n");
		  print(OUT "Queue\n\n");    
		  $njob++;
	      }
	  }
      }
  }
}
print "$njob jobs created\n";

if($opt eq "submit" ){
    print("Submitting ${condor}\n");
    system("condor_submit ${condor}\n");
    system("running200.pl\n");
}
