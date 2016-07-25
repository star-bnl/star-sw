#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my $day = File::Basename::basename(File::Basename::dirname($pwd));
my $run =  File::Basename::basename($pwd);
#print "$pwd => $day => $run\n";
#my @list = glob "/star/data03/daq/2013/15[4-9]/*/st_physics*.daq";
#my @list = glob "/gpfs01/star/scratch/fisyak/daq/2014/*/*/st_physics*.daq";
#
#		
my @globs = ("/star/data03/daq/2014/" . $day . "/" . $run . "*/st*.daq"); 
#my @globs = qw(
#		/star/data03/daq/2014/131/*/st*.daq
#		/star/data03/daq/2014/130/*/st*.daq
#	     ); 
#my @globs = qw(/star/data03/daq/2014/089/15089024/st_physics_15089024_raw_1000025.daq);
my $debug = 0;
my $t = 200;
my $step  =  100000;
#for (my $first =  1; $first < 200; $first += $step) {
my $first =  1;
my $last  =  $first + $step - 1;
print "first = $first, last = $last\n" if ($debug);
sub SPrint ($$$$) {
  my ($line,$file,$f,$l) = @_;
  my $rootf = $file . "_" . $f . "_" . $l . ".MuDst.root";# print "rootf = $rootf\n" if (debug);
  if (-r $rootf) {next;}
  my $blaf  = $file . "_" . $f . "_" . $l . ".bla.root";# print "blaf = $blaf\n" if (debug);
  if (-r $blaf) {next;}
  if (! $debug) {`touch $blaf`;}
  my $chain = "DbV20150316,P2014a,pxlHit,istHit,btof,mtd,mtdCalib,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,-hitfilt,noRunco,noHistos,noTags,-evout";
  if ($pwd =~ /StiCA/) {$chain .= ",StiCA";}
  if ($pwd =~ /KF/) {$chain .= ",KFVertex";}
#  print "chain = $chain\n" if (debug);
  print "string:$line:$f:$l:$chain\n";
}
foreach my $glob (@globs) {
  my @list = glob $glob;
  my $fNo = 0;
  foreach my $line (@list) {
    $fNo++;
    #  chop($line);
    my $file = File::Basename::basename($line,".daq");
    my $cmd = "get_file_list.pl -keys 'events' -cond 'filetype=online_daq,filename=" . $file . ".daq' -limit 1"; print "$cmd\n" if ($debug);
    my $N = `$cmd`; chomp($N); print "N = $N and step = $step\n" if ($debug);
    $last = $N;
    my $f = $first;
    my $l = $last;
    my $fC = $f;
    my $lC = $l;
    my @files = glob $file . "*.MuDst.root"; 
    if ($debug) {
      foreach my $file (@files) {print "$file\n";}
    }
#    die;
    foreach my $mu (@files) {
      my $b = File::Basename::basename($mu,".MuDst.root");
      $b =~ s/_adc//;
      print "$b\n" if ($debug);
      my ($dummy,$dummy,$dummy,$dummy,$dummy,$f1,$l1) = split('_',$b); 
      $l1 =~ s/\.MuDst.root//;
      if ($fC <= $lC and $lC != $N and $f1 != $lC+1) {
	my $fG = $lC+1;
	my $lG = $f1-1;
	print "============ gap : f1 = $f1 l1 = $l1 and fC = $fC lC = $lC => [$fG,$lG]\n" if ($debug);
	if ($fG >= $lG) {next;}
	for (my $i = $fG; $i < $lG; $i += $t) {
	  my $j = $i + $t - 1;
	  if ($j > $lG) {$j = $lG;}
	  print "$line i = $i j = $j\n" if ($debug);
	  SPrint($line,$file,$i,$j);
	}
	die;
      }
      if ($f >= $f1 && $last <= $l1) {$f = $l1 + 1; next;}
      print "$mu => $f1 $l1\n" if ($debug);
      if ($f1 >= $f) {
	$f = $l1 + 1;
      }
      $fC = $f;
      $lC = $l1;
    }
    print "f = $f l = $l\n" if ($debug);
    if ($f >= $l) {next;}
    for (my $i = $f; $i < $l; $i += $t) {
      my $j = $i + $t - 1;
      if ($j > $l) {$j = $l;}
      SPrint($line,$file,$i,$j);
#      last;
    }
#    last;
  }
}
#}
