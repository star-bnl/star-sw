#! /usr/bin/env perl
use File::Basename;
use Cwd;
use File::stat;
use Time::localtime;
#my $timestamp = ctime(stat($fh)->mtime);
#my $glob =  "/net/l404/data/fisyak/Pico/BES-I/AuAu19_production/2011/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/TFG19d/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/14GeV_2019_StiCA/0??/*";
#my $glob =  "./Pico*/???/*/*.picoDst.root";
my $PICOPATH = "";
my $debug = 0;
my $pwd = Cwd::cwd(); print "pwd = $pwd\n" if ($debug);
my $glob;
if ($pwd =~ /dev/) {
  $PICOPATH = "/gpfs01/star/data*";
  $glob = $PICOPATH;
  if    ($pwd =~ /5p75GeV_fixedTarget/) {$glob .= "/reco/production_5p75GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /11p5GeV/)             {$glob .= "/reco/production_11p5GeV_2020/ReversedFullField/dev/20*";}
  
} else {# TFG
  $PICOPATH = "/gpfs01/star/pwg_tasks/tfg02";
  if (! -r  $PICOPATH) {$PICOPATH = "/net/l401/data/scratch1/reco";}
  if (! -r $PICOPATH) {exit 1;}
  $glob = $PICOPATH;
  if    ($pwd =~ /5p75GeV_fixedTarget/) {$glob .= "/2020/TFG19m/RF/5p75GeV_fixedTarget.B";}
  elsif ($pwd =~ /11p5GeV/)             {$glob .= "/2020/TFG19m/RF/11p5GeV.B";}
}
$glob .= "/*/*";
print "glob = $glob\n" if ($debug);
my %Runs= ();
foreach my $run (glob $glob) {
  my $f = File::Basename::basename($run);
  $Runs{$f}++;
  my $ana = $f . "_" . $Runs{$f} . ".root";
  if ( -r $ana) {
    my $mtime = stat($ana)->mtime;
    my $Mtime = ctime($mtime);
#     my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtime, $ctime, $blksize,$blocks) = stat($ana);
    my @files = glob $run . "/*.root";
    if ($#files < 0) {next;}
    my $mtimeA = -1;
    my $MtimeA;
    foreach my $file (@files) {
      my $mtimeR = stat($file)->mtime;
      my $MtimeR = ctime($mtimeR);
      #       my ($devR,$inoR,$modeR,$nlinkR,$uidR,$gidR,$devR, $sizeR, $atimeR, $mtimeR, $ctimeR, $blksizeR,$blocksR) = stat($run);
      my $dt = $mtime - $mtimeR;
      #       print "$ana: $mtime,$Mtime  $file: $mtimeR,$MtimeR dt = $dt\n";
      #       my @list = `ls -l $ana $file`; print "@list\n";
      if ( $mtimeR > $mtimeA) { $mtimeA =  $mtimeR; $MtimeA = $MtimeR;}
    }
    my $dt = $mtime - $mtimeA;
    print "$ana $mtime,$Mtime, $run $mtimeA,$MtimeA, dt = $dt\n" if ($debug);
    if ($dt > 0) {next;}
    my $cmd = "mv ". $ana ." ". $ana .".BAK";
    print "$cmd \n" if ($debug);
    my $flag = system($cmd);
  };
  print "string:$run:$ana\n";
#  last;
#  die;
}
