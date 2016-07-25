#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#my @list = glob "/star/data03/daq/2013/15[4-9]/*/st_physics*.daq";
#my @list = glob "/gpfs01/star/scratch/fisyak/daq/2014/*/*/st_physics*.daq";
#
my @globs = qw(
		/star/data03/daq/2014/108/*/st_physics_15*.daq
		/star/data03/daq/2014/109/*/st_physics_15*.daq
		/star/data03/daq/2014/110/*/st_physics_15*.daq
	     ); 
#my @globs = qw(/star/data03/daq/2014/089/15089024/st_physics_15089024_raw_1000025.daq);
my $debug = 0;
my $step  =  200;
for (my $first =  1; $first < 200; $first += $step) {
my $last  =  $first + $step - 1;
print "first = $first, last = $last\n" if ($debug);
foreach my $glob (@globs) {
  my @list = glob $glob;
  my $fNo = 0;
  foreach my $line (@list) {
    $fNo++;
    my $f = $first;
    my $l = $last;
    #  chop($line);
    my $file = File::Basename::basename($line,".daq");
    my @files = glob $file . "*.MuDst.root";
    foreach my $mu (@files) {
      my $b = File::Basename::basename($mu,".MuDst.root");
      print "$b\n" if ($debug);
      my ($dummy,$dummy,$dummy,$dummy,$dummy,$f1,$l1) = split('_',$mu); 
      $l1 =~ s/\.MuDst.root//;
      if ($f >= $f1 && $last <= $l1) {$f = $l1 + 1; next;}
      print "$mu => $f1 $l1\n" if ($debug);
      if ($f1 >= $f) {
	$f = $l1 + 1;
      }
    }
    print "f = $f l = $l\n" if ($debug);
    if ($f >= $l) {next;}
    $rootf = $file . "_" . $f . "_" . $l . ".MuDst.root";
    if (-r $rootf) {next;}
    my $chain = "DbV20150316,P2014a,pxlHit,istHit,btof,mtd,mtdCalib,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,-hitfilt,noRunco,noHistos,noTags";
    if ($pwd =~ /StiCA/) {$chain .= ",StiCA";}
    if ($pwd =~ /KF/) {$chain .= ",KFVertex";}

    print "string:$line:$f:$l:$chain\n";
    #  last;
    #  }
    #  last;
  }
}
}
