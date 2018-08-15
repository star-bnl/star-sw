#! /usr/bin/env perl
use File::Basename;
use Cwd;
use File::stat;
my $pwd = cwd();
my $day = File::Basename::basename(File::Basename::dirname($pwd));
my $run =  File::Basename::basename($pwd);
#my $dir = "/gpfs01/star/subsys-tpc/fisyak/reco/2014/50M/SL15StiCAKFV/" . $day . "/" . $run;
#my $dir = "/net/l401/data/scratch2/fisyak/MuDst/2016/" . $day . "/" . $run;
my $debug = 0;
#my $dir = "/gpfs01/star/pwg/fisyak/MuDst/2016/" . $day . "/" . $run; print "dir = $dir\n" if ($debug);
#$dir = "./";
#my $dir = "/gpfs02/eic/ayk/STAR/reco/MuDst/2010/" . $day . "/" . $run; print "dir = $dir\n" if ($debug);
my $dir = "/gpfs02/eic/ayk/STAR/reco/MuDst/2016/" . $day . "/" . $run; print "dir = $dir\n" if ($debug);
my %Hash = ();
my @list = glob "$dir" . "/*.MuDst.root";
my $NJobs = 0;
my ($st,$physcs,$adc,$r,$raw,$p,$f1,$l1);
my $file;
foreach my $line (@list) { print "$line\n" if ($debug);
  my $b = File::Basename::basename($line,".MuDst.root");
#  $b =~ s/_adc//;  print "$b\n" if ($debug);
  if ($b =~ /_adc/) {
    ($st,$physics,$adc,$r,$raw,$p,$f1,$l1) = split('_',$b); print "r = $r, p = $p, f1 = $f1, l1 = $l1\n" if ($debug);
    $file = $st . "_" . $physics . "_" . $adc . "_" . $r . "_" . $raw . "_" . $p; print "file = $file \n" if ($debug);
  } else {
    ($st,$physics,$r,$raw,$p,$f1,$l1) = split('_',$b); print "r = $r, p = $p, f1 = $f1, l1 = $l1\n" if ($debug);
    $file = $st . "_" . $physics  . "_" . $r . "_" . $raw . "_" . $p; print "file = $file \n" if ($debug);
  }
#  if ($run ne $r) {die "run != r";}
#  my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks) = File::stat($line);
  $st = stat($line);			   
#  print "$dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks\n";
  my $size = $st->size;
  print "$line size = $size\n" if ($debug);
#  die;			   
  if ($size < 10000) {next;}
  my $inode = stat($line);
  my $ctime = $inode->ctime;
  
  if (! $Hash{$file}) {$Hash{$file} = $ctime;}
  elsif ($Hash{$file} < $ctime)  {$Hash{$file} = $ctime;}
}
foreach my $key ( sort keys %Hash ) {
  print "$key => $Hash{$key}\n" if ($debug);
# st_15131004_raw_3000031.picoDst.root
# st_15131004_raw_3000031.picoDst.root
  my $rootf = $key . ".picoDst.root"; print "rootf = $rootf\n" if ($debug);
  if (-r $rootf) {
    my $inode = stat($rootf);
    my $ctime = $inode->ctime;
    print "$rootf ctime = $ctime, MuDst ctime =  $Hash{$key}\n" if ($debug);
    if ($ctime >= $Hash{$key}) {
      next;
    }
    my $date = `date +%m%d%y`;
    chop($date);
    my $cmd = "mv " . $rootf . " " . $rootf . ".HOLD." . $date; print "$cmd\n" if ($debug);
    my $flag = system($cmd); if ($flag) {die "flag = $flag";}
  }
  $NJobs++;
  print "string:$dir:$key\n";
}
if (! $NJobs) {die "No.jobs";}
