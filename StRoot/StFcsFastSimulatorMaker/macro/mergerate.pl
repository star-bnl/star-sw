#!/usr/bin/perl -w

$dir="fcsMc/pythia.jet.vz0";
$name="pythia_jet_vz0";
$hist="qahist";

$nfile=0;
$out="$name.$hist.root";
$cmd="cd $dir; hadd -f $out ";
if(-e "$out") {`/bin/rm $out`};

print "Searching $dir for .$hist.root\n";

opendir(DIR,"$dir");
my @files1= grep {/.$hist.root/} readdir DIR;
my @files2= grep {/_run/} @files1;
foreach $file (@files2) {
    $cmd = "$cmd $file";
    $nfile++;
    print "$nfile found for $name in $dir/$name\n";
}
print "cmd=$cmd\n";
$out=`$cmd`;
print "$out\n";
