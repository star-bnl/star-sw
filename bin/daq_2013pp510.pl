#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#my @list = glob "/star/data03/daq/2013/15[4-9]/*/st_physics*.daq";
#my @list = glob "/gpfs01/star/scratch/fisyak/daq/2014/*/*/st_physics*.daq";
#
my @globs = qw(
		/star/data03/daq/2013/07*/*/st_W*.daq
	     ); 
#my @globs = qw(/star/data03/daq/2014/089/15089024/st_physics_15089024_raw_1000025.daq);
my $debug = 0;
my $step  =  1000;
#for (my $first =  901; $first < 2500; $first += $step) {
my $first = 1;
my $last  =  $first + $step - 1;
print "first = $first, last = $last\n" if ($debug);
foreach my $glob (@globs) {
  my @list = glob $glob;
  my $fNo = 0;
  foreach my $line (@list) {
    $fNo++;
    my $f = $first;
    my $l = $last;
#    chop($line);
    my $file = File::Basename::basename($line,".daq");
    my $cmd = "get_file_list.pl -keys 'events' -cond 'filetype=online_daq,filename='" . $file . ".daq -limit 1";
    my $N = `$cmd`; chomp($N); print "N = $N\n" if ($debug);
    if ($l > $N) {$l = $N;}
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
#    my $chain = "DbV20150316,P2014a,pxlHit,istHit,btof,mtd,mtdCalib,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,-hitfilt,noRunco,noHistos,noTags";
    my $chain = "DbV20140222,pp2013a,mtd,btof,fmsDat,fgt,fgtPoint,beamline,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,-hitfilt,noRunco,noHistos,noTags";
    if ($pwd =~ /StiCAKF/) {$chain .= ",StiCA,KFVertex";}
    elsif ($pwd =~ /StiCA/) {$chain .= ",StiCA";}
    else                    {$chain .= ",VFPPVnoCTB";}
    print "string:$line:$f:$l:$chain\n";
  }
#}
}
