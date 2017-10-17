#! /usr/bin/env perl
use File::Basename;
use Cwd;
use File::stat;
my $pwd = cwd();
my $day = File::Basename::basename(File::Basename::dirname($pwd));
my $run =  File::Basename::basename($pwd);
my $dir = "/star/subsys/tpc/fisyak/reco/2014/50M/SL15StiCAKFV/" . $day . "/" . $run;
my $debug = 0;
my %Hash = ();
my @list = glob "$dir" . "/*.MuDst.root";
my $NJobs = 0;
my ($st,$physcs,$adc,$r,$raw,$p,$f1,$l1);
my $file;
foreach my $line (@list) {
  my $b = File::Basename::basename($line,".MuDst.root");
#  $b =~ s/_adc//;  print "$b\n" if ($debug);
  if ($b =~ /_adc/) {
    ($st,$physics,$adc,$r,$raw,$p,$f1,$l1) = split('_',$b); print "r = $r, p = $p, f1 = $f1, l1 = $l1\n" if ($debug);
    $file = $st . "_" . $physics . "_" . $adc . "_" . $r . "_" . $raw . "_" . $p; print "file = $file \n" if ($debug);
  } else {
    ($st,$physics,$r,$raw,$p,$f1,$l1) = split('_',$b); print "r = $r, p = $p, f1 = $f1, l1 = $l1\n" if ($debug);
    $file = $st . "_" . $physics  . "_" . $r . "_" . $raw . "_" . $p; print "file = $file \n" if ($debug);
  }
  if ($run ne $r) {die "run != r";}
#  my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks) = stat($line);
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
