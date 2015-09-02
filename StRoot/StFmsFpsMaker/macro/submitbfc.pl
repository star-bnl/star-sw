#!/usr/bin/perl -w

my $opt="none";
if($#ARGV==0) {
    $opt=$ARGV[0];
    print "Option = $opt\n";
}else{
    print "Option = none, just searching files. To submit job, add option submit\n";
}

my $dir="/star/u/akio/pwg/fms2015/data";
#my $dir="/star/data05/scratch/sheppel/daq/";
my $exe="runbfc";
my $nfile=0;

my $logdir="log";
my $condordir="condor";
my $cwd=$ENV{'PWD'};

if (! -e "$logdir")   {system("/bin/mkdir $logdir");}
if (! -e "$condordir") {system("/bin/mkdir $condordir");}

my $condor="$condordir/submitbfc.txt";
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
my @files = grep {/\.daq/} readdir DIR;
for $file (@files){	
    $nfile++;
    print(OUT "Arguments = \"$dir $file\"\n");
    print(OUT "Log    = $cwd/$logdir/$file.log\n");
    print(OUT "Output = $cwd/$logdir/$file.log\n");
    print(OUT "Error  = $cwd/$logdir/$file.log\n");
    print(OUT "Queue\n\n");    
}
print "$nfile files found\n";

if($opt eq "submit" ){
    print("Submitting ${condor}\n");
    system("condor_submit ${condor}\n");
    system("running200.pl\n");
}
