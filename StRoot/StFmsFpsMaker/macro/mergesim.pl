#!/usr/bin/perl -w

@dirs= ("sim_nomerge","sim_merge");
@pid = ("electron","gamma","mu-","pi-","pi0");

foreach $dir (@dirs){
    foreach $p (@pid) {
	$nfile=0;
	$out="test_$p.fmsfps.root";
	$cmd="cd $dir;hadd -f $out ";
	if(-e "$dir/$out") {`/bin/rm $dir/$out`};
	opendir(DIR,$dir);
	my @files1= grep {/.fmsfps.root/} readdir DIR;
	my @files2= grep {/test_/} @files1;
	my @files3= grep {/_run/} @files2;
	my @files4= grep {/$p/}  @files3;
	foreach $file (@files4) {
	    $cmd = "$cmd $file";
	    $nfile++;
	}
	print "$nfile found for $p in $dir\n";
#    print "cmd=$cmd\n";
	$out=`$cmd`;
#x	print "$out\n";
    }
}
