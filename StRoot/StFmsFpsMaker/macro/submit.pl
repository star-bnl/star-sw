#!/usr/bin/perl -w

my $opt="none";
if($#ARGV==0) {
    $opt=$ARGV[0];
    print "Option = $opt\n";
}else{
    print "Option = none, just searching files. To submit job, add option submit\n";
}

my $dir="/star/u/akio/pwg/fms2015/mudst";
#my $dir="/gpfs01/star/subsysg/FPS/sheppel/mudst/";
#my $nfile=100;
#my $nev=1000;
my $nfile=10;
my $nev=10000;
my $outdir="hist";
my $readmudst=0;

my $exe="runmudst";
my $logdir="log";
my $condordir="condor";
my $cwd=$ENV{'PWD'};

if (! -e "$logdir")   {system("/bin/mkdir $logdir");}
if (! -e "$condordir") {system("/bin/mkdir $condordir");}

my $condor="$condordir/submit.txt";
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

opendir(DIR,$dir);
my @files = grep {/.MuDst.root/} readdir DIR;
for $file (@files){	
    $nfound++;
    for (my $ifile=0; $ifile < $nfile; $ifile++) {
	$log="$cwd/$logdir/$file.$ifile.nmg.log";
	print(OUT "Arguments = \"$dir $file $ifile $nev ${outdir}_nomerge 0 $readmudst\"\n");
	print(OUT "Log    = $log\n");
	print(OUT "Output = $log\n");
	print(OUT "Error  = $log\n");
	print(OUT "Queue\n\n");    
	$njob++;

	$log="$cwd/$logdir/$file.$ifile.mg.log";
	print(OUT "Arguments = \"$dir $file $ifile $nev ${outdir}_merge 1 $readmudst\"\n");
	print(OUT "Log    = $log\n");
	print(OUT "Output = $log\n");
	print(OUT "Error  = $log\n");
	print(OUT "Queue\n\n");    
	$njob++;
    }
}
print "$nfound files found, creating $njob jobs\n";

if($opt eq "submit" ){
    print("Submitting ${condor}\n");
    system("condor_submit ${condor}\n");
    system("running200.pl\n");
    system("merge2.pl submit\n");
}
