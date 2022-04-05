#!/usr/bin/perl -w

@dirs= ("out");
@pid = ("ele","pos");
@pt = ("0.6", "0.8", "1.0");
@vz = ("0", "50","-50");

$hist="trgqa";

foreach $dir (@dirs){
    foreach $p (@pid) {
	foreach $pp (@pt) {	    
	    foreach $vz (@vz) {	    
		$nfile=0;
		$name="$p.pt$pp.vz$vz";
		$out="$name.$hist.root";
		$cmd="cd $dir/$name; hadd -f $out ";
		if(-e "$out") {`/bin/rm $out`};
		opendir(DIR,"$dir/$name");
		my @files1= grep {/.$hist.root/} readdir DIR;
		my @files2= grep {/$name./} @files1;
		my @files3= grep {/.run/} @files2;
		foreach $file (@files3) {
		    $cmd = "$cmd $file";
		    $nfile++;
		}
		print "$nfile found for $name in $dir/$name\n";
		print "cmd=$cmd\n";
		$out=`$cmd`;
		print "$out\n";
	    }
	}
    }
}
