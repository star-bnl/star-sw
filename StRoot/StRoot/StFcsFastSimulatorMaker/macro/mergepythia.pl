#!/usr/bin/perl -w

@dirs= ("out");
@pid = ("dy","dybg");
@vz = ("0");
#@mask = ("0.0300");
@mask = ("0.0010");
#@mask = ("0.3000","0.1000",         "0.0100","0.0030","0.0010");
#@mask = ("0.3000","0.1000","0.0300","0.0100","0.0030","0.0010");

$hist="trgqa";

foreach $dir (@dirs){
    foreach $p (@pid) {
	foreach $vz (@vz) {	    
	    foreach $m (@mask) {	    
		$nfile=0;
		$name="pythia.$p.vz$vz";
		$thr="thr$m";
		$out="$name.$thr.$hist.root";
		$cmd="cd $dir/$name; hadd -f $out ";
		if(-e "$out") {`/bin/rm $out`};
		opendir(DIR,"$dir/$name");
		my @files1= grep {/.$hist.root/} readdir DIR;
		my @files2= grep {/$name./} @files1;
		my @files3= grep {/$thr./} @files2;
		my @files4= grep {/.run/} @files3;
		foreach $file (@files4) {
		    $cmd = "$cmd $file";
		    $nfile++;
		}
		print "$nfile found for $name in $dir/$name with $thr\n";
		print "cmd=$cmd\n";
		$out=`$cmd`;
		print "$out\n";
	    }
	}
    }
}
