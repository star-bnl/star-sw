#!/usr/bin/perl -w

my $condordir="condor";
my $logdir="log";
my $cwd=$ENV{'PWD'};

my $opt="none";
if($#ARGV==0) {
    $opt=$ARGV[0];
    print "Option = $opt\n";
}else{
    print "Option = none, just searching files. To submit job, add option submit. Or exe for just do it here\n";
}

@dirs= ("hist_nomerge","hist_merge");
%cmd2=();

$exe="$condordir/histmerge.exe";
open(EXE,"> $exe");

$condor="$condordir/histmerge.condor";
open(CONDOR,"> $condor");
print(CONDOR "Universe     = vanilla\n");
print(CONDOR "notification = never\n");
print(CONDOR "getenv       = True\n");
print(CONDOR "Requirements = CPU_Experiment == \"star\"\n");
print(CONDOR "+Experiment  = \"star\"\n");
print(CONDOR "+Job_Type    = \"cas\"\n");
print(CONDOR "\n");

foreach $dir (@dirs){
    $cmd2{"$dir"}="/bin/rm $dir/fmsfps.root; hadd $dir/fmsfps.root ";
    $nfile=0;
    $nrun=0;
    $nmudst=0;
    %runs=();
    %cmd=();
    opendir(DIR,$dir);
    my @files1= grep {/.fmsfps.root/} readdir DIR;
    my @files2= grep {/st_/} @files1;      
    my @files3= grep {!/.merged./} @files2;      
    foreach $file (@files3) {		
	$run = substr($file,7,8);
	$evb = substr($file,20,7);
	$key = "${run}_${evb}";	
	#print "$file $run $evb $key\n";
	if( ! exists $runs{"$run"}) {
	    $runs{"$run"}=$nrun;
	    $nrun++;
	}
	if( ! exists $cmd{"$key"} ) {
	    $outfile="$dir/st_fms_${run}_raw_${evb}.merged.fmsfps.root";
	    $cmd{"$key"}="/bin/rm $outfile \n hadd $outfile";
	    $cmd2{"$dir"}="$cmd2{\"$dir\"} $outfile";
	    $nmudst++;	    
	}	
	$cmd{"$key"}="$cmd{\"$key\"} $dir/$file";
	$nfile++;
    }

    while (($key, $command) = each(%cmd)) {
	$cmdfile="$condordir/histmerge_${dir}_${key}";
	print "Creating $cmdfile\n";
	open(OUT, "> $cmdfile");
	print(OUT "#!/bin/csh\n");
	print(OUT "$command\n");
	close(OUT);
	`/bin/chmod +x $cmdfile`;
	
	print(EXE "$cmdfile\n");
	$log="$cwd/$logdir/histmerge_${dir}_${key}.log";
	print(CONDOR "Executable = $cmdfile\n");
	print(CONDOR "Log    = $log\n");
	print(CONDOR "Output = $log\n");
	print(CONDOR "Error  = $log\n");
	print(CONDOR "Queue\n\n");    
    }
    print(EXE "$cmd2{$dir}");
    print "$nfile histo found for $nmudst mudsts and $nrun runs in $dir\n";
}
close(EXE);
close(CONDOR);
print "Created $condor and $exe\n";

if($opt eq "submit" ){
    print("Submitting ${condor}\n");
    system("condor_submit ${condor}\n");
    system("running200.pl\n");
}

if($opt eq "exe" ){
    print("Executing ${exe}\n");
    `/bin/chmod +x $exe`;
    system("${exe}\n");
}

if($opt eq "submit" || $opt eq "2ndpass"){
    foreach $dir (@dirs){
	$c=$cmd2{"$dir"};
	print("$c\n");
	`$c`;
    }
}
