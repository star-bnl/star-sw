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
my $dayMin =  0;
my $dayMax =  0;
if ($pwd =~ /dev/) {
  $PICOPATH = "/gpfs01/star/data*";
  $glob = $PICOPATH;
  if    ($pwd =~ /5p75GeV_fixedTarget/) {$glob .= "/reco/production_5p75GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /11p5GeV.C/)           {$glob .= "/reco/production_11p5GeV_2020/ReversedFullField/dev/20*"; $dayMin = 42;}
  elsif ($pwd =~ /11p5GeV/)             {$glob .= "/reco/production_11p5GeV_2020/ReversedFullField/dev/20*"; $dayMax = 41;}
  elsif ($pwd =~ /13p5GeV_fixedTarget/) {$glob .= "/reco/production_13p5GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /19p5GeV_fixedTarget/) {$glob .= "/reco/production_19p5GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /31p2GeV_fixedTarget/) {$glob .= "/reco/production_31p2GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /7p3GeV_fixedTarget/)  {$glob .= "/reco/production_7p3GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /9p2GeV/)              {$glob .= "/reco/production_9p2GeV_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /9p8GeV_fixedTarget/)  {$glob .= "/reco/production_9p8GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  
} else {# TFG
  $PICOPATH = "/gpfs01/star/pwg_tasks/tfg02";
  if (! -r  $PICOPATH) {$PICOPATH = "/net/l401/data/scratch1/reco";}
  if (! -r $PICOPATH) {exit 1;}
  $glob = $PICOPATH;
  if    ($pwd =~ /11GeV/)               {$glob .= "/2010/11GeV";}
  elsif ($pwd =~ /19GeV_2010/)          {$glob .= "/2010/19GeV";}
  elsif ($pwd =~ /7GeV/)                {$glob .= "/2010/7GeV";}

  elsif ($pwd =~ /27GeV/)               {$glob .= "/2011/27GeV";}

  elsif ($pwd =~ /3p85GeV_fixedTarget/) {$glob .= "/2018/3p85GeV_fixedTarget";}

  elsif ($pwd =~ /14GeV/)               {$glob .= "/2019/14GeV_2019_TFG19e";}
  elsif ($pwd =~ /19GeV_2019/)          {$glob .= "/2019/19GeV_2019_TFG19e";}
  elsif ($pwd =~ /31GeV_fixedTarget_2019/) {$glob .= "/2019/31GeV_fixedTarget_2019_TFG19l.Minuit";}
  elsif ($pwd =~ /4p59GeV_fixedTarget_2019/) {$glob .= "/2019/4p59GeV_fixedTarget_2019_TFG19e";}
  elsif ($pwd =~ /7\.3GeV_fixedTarget_2019/) {$glob .= "/2019/7.3GeV_fixedTarget_2019_TFG19e";}
  elsif ($pwd =~ /7p7GeV_2019/)         {$glob .= "/2019/7p7GeV_2019_TFG19e";}
  elsif ($pwd =~ /9p2GeV_2019/)         {$glob .= "/2019/9p2GeV_2019_TFG19e";}

  elsif ($pwd =~ /5p75GeV_fixedTargetC/){$glob .= "/2020/TFG20a/RF/5p75GeV_fixedTarget";}
  elsif ($pwd =~ /5p75GeV_fixedTarget/) {$glob .= "/2020/TFG19m/RF/5p75GeV_fixedTarget.B";}
  elsif ($pwd =~ /11p5GeV.C/)           {$glob .= "/2020/TFG20a/RF/11p5GeV";}
  elsif ($pwd =~ /11p5GeV/)             {$glob .= "/2020/TFG19m/RF/11p5GeV.B";}
  elsif ($pwd =~ /31p2GeV_fixedTarget/) {$glob .= "/2020/TFG20a/RF/31p2GeV_fixedTarget";}
  elsif ($pwd =~ /9p8GeV_fixedTarget/)  {$glob .= "/2020/TFG20a/RF/9p8GeV_fixedTarget";}
  elsif ($pwd =~ /9p2GeVb/)              {$glob .= "/2020/TFG20a/RF/9p2GeVb";}
  elsif ($pwd =~ /9p2GeV/)              {$glob .= "/2020/TFG20a/RF/9p2GeV";}
  elsif ($pwd =~ /13p5GeV_fixedTarget/) {$glob .= "/2020/TFG20a/RF/13p5GeV_fixedTarget";}
  elsif ($pwd =~ /19p5GeV_fixedTarget/) {$glob .= "/2020/TFG20a/RF/19p5GeV_fixedTarget";}
  elsif ($pwd =~ /7p3GeV_fixedTarget/)  {$glob .= "/2020/TFG20a/RF/7p3GeV_fixedTarget";}
  elsif ($pwd =~ /9p2GeV/)              {$glob .= "/2020/TFG20a/RF/9p2GeV";}
  
}
$glob .= "/*/*";
print "days = $dayMin  - $dayMax : glob = $glob\n" if ($debug);
my %Runs= ();
foreach my $run (glob $glob) {
  my $f = File::Basename::basename($run);
  my $day = int ($f/1000);       # print "day = $day\n";
  my $year = int ($day/1000);    # print "year = $year\n";
  $day -=  1000*$year;           # print "day = $day\n";
  if ($dayMin > 0 && $day < $dayMin) {next;}
  if ($dayMax > 0 && $day > $dayMax) {next;}
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
    print "$day, $ana $mtime,$Mtime, $run $mtimeA,$MtimeA, dt = $dt\n" if ($debug);
    if ($dt > 0) {next;}
    my $cmd = "mv ". $ana ." ". $ana .".BAK";
    print "$cmd \n" if ($debug);
    my $flag = system($cmd);
  };
  my $picoGlob = $run . "/*picoDst.root";
  my @picos = glob $picoGlob;
  if ($#picos > -1) {
    print "string:$run:$ana\n";
  }
#  last;
#  die;
}
