#!/usr/bin/perl -w

$hist="dipi0";
my $cwd=$ENV{'PWD'};

my $beam="pptrans";
my $opt="none";
if($#ARGV>=0) {$beam=$ARGV[0];}
if($#ARGV>=1) {$opt=$ARGV[1];}
print "Beam = $beam (pptrans,pplong2,pAu1,pAu2 or pAl)\n";
print "Option = $opt (none, submit, 2ndpass, 3rdpass, exe, exe2)\n";

my $logdir="log/tree/$beam";
if(! -e $logdir){ `mkdir -p $logdir`; }

my $condordir="condor/$beam";
if(! -e $condordir){ `mkdir -p $condordir`; }

$indir="hist_$beam";

$outdir="hist/$beam";
if(! -e $outdir){ `mkdir -p $outdir`; }

$exe="$condordir/runmerge.exe";
open(EXE,"> $exe");

#read good run list
$goodrunfile="www/goodruns.$beam.txt";
if(! -e $goodrunfile){
    print "Cannot find $goodrunfile\n";
    exit;
}
open(IN,"$goodrunfile");
@goodruns=<IN>;
close(IN);
%GOODRUN=();
foreach $run (@goodruns){    
    $run=~s/\n//;
    $GOODRUN{"$run"}=1;
}

$condor="$condordir/readtree.$beam.condor";
open(CONDOR,"> $condor");
print(CONDOR "Universe     = vanilla\n");
print(CONDOR "notification = never\n");
print(CONDOR "getenv       = True\n");
print(CONDOR "Requirements = CPU_Experiment == \"star\"\n");
print(CONDOR "+Experiment  = \"star\"\n");
print(CONDOR "+Job_Type    = \"cas\"\n");
print(CONDOR "\n");

$condor2="$condordir/mergeday.$beam.condor";
open(CONDOR2,"> $condor2");
print(CONDOR2 "Universe     = vanilla\n");
print(CONDOR2 "notification = never\n");
print(CONDOR2 "getenv       = True\n");
print(CONDOR2 "Requirements = CPU_Experiment == \"star\"\n");
print(CONDOR2 "+Experiment  = \"star\"\n");
print(CONDOR2 "+Job_Type    = \"cas\"\n");
print(CONDOR2 "\n");

$nrun=0;
$nday=0;
%runs=();
%days=();

$merge="$condordir/merge.exe";
print "Creating $merge\n";
open(MGEXE,"> $merge");
print(MGEXE "#!/bin/csh\n");

$mergeall="$condordir/merge.all.exe";
print "Creating $mergeall\n";
open(OUTALL,"> $mergeall");
print(OUTALL "#!/bin/csh\n");
print(OUTALL "echo Removing $outdir/$hist.root\n");
print(OUTALL "/bin/rm -f $outdir/$hist.root\n");
print(OUTALL "echo Adding to $outdir/$hist.root\n");
print(OUTALL "cd $outdir; pwd; hadd -n 5 $hist.root");

opendir(DIR,$indir);
my @files= grep {/.tree.root/} readdir DIR;
foreach $file (@files) {		
    $run = substr($file,0,8);
    $day = substr($run,0,5);
    $fsize = -s "$indir/$file";
    print "$day $run $indir/$file size=$fsize\n";
    if( ! defined($GOODRUN{"$run"})) {next;}
    $mergeday="$condordir/merge.$day.exe";

    if( ! exists $days{"$day"} ) {
	$nday++;	
	$days{"$day"}=$nday;
	$outfileday="${day}_day.$hist.root";

	print(MGEXE "$mergeday\n");
	print(OUTALL " $outfileday");
	
	print "Creating $mergeday\n";
	open(OUT, "> $mergeday");
	print(OUT "#!/bin/csh\n");
	print(OUT "echo Removing $outdir/$outfileday\n");
	print(OUT "/bin/rm -f $outdir/$outfileday\n");
	print(OUT "echo Adding to $outdir/$outfileday\n");	
	print(OUT "cd $outdir; pwd; hadd -n 5 $outfileday");
	close(OUT);

	$log="$cwd/$logdir/mergeday_${beam}_${day}.log";
        print(CONDOR2 "Executable = $mergeday\n");
        print(CONDOR2 "Log    = $log\n");
        print(CONDOR2 "Output = $log\n");
        print(CONDOR2 "Error  = $log\n");
	print(CONDOR2 "Queue\n\n");

    }
    if( ! exists $runs{"$run"} ) {
	$nrun++;
	$runs{"$run"}=$nrun;
	$cmdfilerun="$condordir/${run}.exe";
	$outfilerun="${run}.$hist.root";
	
	open(OUT, ">> $mergeday");
	print(OUT " $outfilerun");
	close(OUT);

	open(OUT, "> $cmdfilerun");
        print(OUT "#!/bin/csh\n");
	print(OUT "setenv SUBMITTINGDIRECTORY \$PWD\n");
	print(OUT "mkdir -p /tmp/akio/submit_tree/$beam\n");
	print(OUT "cp -f $indir/${run}.*.tree.root /tmp/akio/submit_tree/$beam/\n");
	print(OUT "cd /tmp/akio/submit_tree\n");
	print(OUT "ln -s \$SUBMITTINGDIRECTORY/.\$STAR_HOST_SYS\n");
	print(OUT "ln -s \$SUBMITTINGDIRECTORY/runTree.C\n");
        print(OUT "root -b -q runTree.C\'\(\"$beam\",\"$run\"\)\'\n");
        print(OUT "/bin/rm -f \$SUBMITTINGDIRECTORY/$outdir/$outfilerun\n");
        print(OUT "/bin/mv $beam/$outfilerun \$SUBMITTINGDIRECTORY/$outdir/\n");
	print(OUT "/bin/rm -f /tmp/akio/submit_tree/$beam/${run}.*.tree.root\n");
	close(OUT);
	`/bin/chmod +x $cmdfilerun`;

	$log="$cwd/$logdir/readtree_${beam}_${run}.log";
	print(CONDOR "Executable = $cmdfilerun\n");
	print(CONDOR "Log    = $log\n");
	print(CONDOR "Output = $log\n");
	print(CONDOR "Error  = $log\n");
	print(CONDOR "Queue\n\n");    

	print(EXE "root -b -q runTree.C\'\(\"$indir\",\"$run\"\)\'; ");
	print(EXE "/bin/mv $indir/$outfilerun $outdir/\n");
    }    
}

while (($day, $value) = each(%days)) {
    $mergeday="$condordir/merge.$day.exe";	    
    open(OUT, ">> $mergeday");
    print(OUT "\n");
    close(OUT);
    `/bin/chmod +x $mergeday`;
    print "Created $mergeday\n";
}

print(OUTALL "\n");
close(OUTALL);
`/bin/chmod +x $mergeall`;
print "Created $mergeall\n";

print(MGEXE "$mergeall\n");
close(MGEXE);
`/bin/chmod +x $merge`;
print "Created $merge\n";

close(EXE);
`/bin/chmod +x $exe`;
print "Created $exe\n";

close(CONDOR);
print "Created $condor\n";

close(CONDOR2);
print "Created $condor2\n";

if($opt eq "submit" ){
    print("Submitting ${condor}\n");
    system("condor_submit ${condor}\n");
    system("running200.pl\n");
}

if($opt eq "exe" ){
    print("Executing ${exe}\n");
    system("${exe}\n");
}

if($opt eq "submit" || $opt eq "2ndpass"){
    print("Submitting ${condor2}\n");
    system("condor_submit ${condor2}\n");
    system("running200.pl\n");
}

if($opt eq "exe" || $opt eq "exe2"){
    print("$merge\n");
    `$merge`;
}

if($opt eq "submit" || $opt eq "exe" || $opt eq "2ndpass" || $opt eq "exe2" || $opt eq "3rdpass"){
    print("$mergeall\n");
    `$mergeall`;
}

