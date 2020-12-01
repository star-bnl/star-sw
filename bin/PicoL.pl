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
if ($#ARGV >= 0) {
  $debug = $ARGV[0];
}
my $pwd = Cwd::cwd(); print "pwd = $pwd\n" if ($debug);
my $glob;
my $dayMin =  0;
my $dayMax =  0;
my $year = "y2020";
my $Njobs = 0;
if    ($pwd =~ /2020/) { $year = "y2020";}
elsif ($pwd =~ /2019/) { $year = "y2019";} 
elsif ($pwd =~ /2018/) { $year = "y2018";} 
elsif ($pwd =~ /2017/) { $year = "y2017";} 
elsif ($pwd =~ /2016/) { $year = "y2016";} 
elsif ($pwd =~ /2015/) { $year = "y2015";} 
elsif ($pwd =~ /2014/) { $year = "y2014";} 
elsif ($pwd =~ /2013/) { $year = "y2013";} 
elsif ($pwd =~ /2012/) { $year = "y2012";} 
elsif ($pwd =~ /2011/) { $year = "y2011";} 
elsif ($pwd =~ /2010/) { $year = "y2010";} 
if ($pwd =~ /dev/ or $pwd =~ /P20ic_calib/ or $pwd =~ /P20ic/) {
  $PICOPATH = "/gpfs01/star/data*";
  if    ($pwd =~ /2020\/5p75GeV_fixedTarget/) {$glob = "/reco/production_5p75GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /2020\/11p5GeV.C/)           {$glob = "/reco/production_11p5GeV_2020/ReversedFullField/dev/20*"; $dayMin = 42;}
  elsif ($pwd =~ /2020\/11p5GeV/)             {$glob = "/reco/production_11p5GeV_2020/ReversedFullField/dev/20*"; $dayMax = 41;}
  elsif ($pwd =~ /2020\/13p5GeV_fixedTarget/) {$glob = "/reco/production_13p5GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /2020\/19p5GeV_fixedTarget/) {$glob = "/reco/production_19p5GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /2020\/31p2GeV_fixedTarget/) {$glob = "/reco/production_31p2GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /2020\/7p3GeV_fixedTarget/)  {$glob = "/reco/production_7p3GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /2020\/9p2GeVc/)             {$glob = "/reco/production_9p2GeV_2020c/ReversedFullField/dev/2020";}
  elsif ($pwd =~ /2020\/9p2GeVb/)             {$glob = "/reco/production_9p2GeV_2020b/ReversedFullField/dev/2020";}
  elsif ($pwd =~ /2020\/9p2GeV/)              {$glob = "/reco/production_9p2GeV_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /2020\/9p8GeV_fixedTarget/)  {$glob = "/reco/production_9p8GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /2019\/19GeV_2019/)          {$glob = "/reco/production_19GeV_2019/ReversedFullField/P20ic_calib/2019";}
  elsif ($pwd =~ /2017\/pp500/)               {$glob = "/reco/pp500_production_2017/ReversedFullField/P20ic/2017";}
} else {# TFG
  $PICOPATH = "/gpfs01/star/pwg_tasks/tfg02"; 
#  if (! -r  $PICOPATH) {$PICOPATH = "/net/l401/data/scratch1/reco";}
  if (! -r  $PICOPATH) {$PICOPATH = "/hlt/cephfs/Pico";}
  print "PICOPATH = $PICOPATH\n" if ($debug);
  if (! -r $PICOPATH) {exit 1;}
  if    ($pwd =~ /2010\/11GeV/)               {$glob = "/2010/11GeV";}
  elsif ($pwd =~ /2010\/19GeV/)               {$glob = "/2010/19GeV";}
  elsif ($pwd =~ /2010\/7GeV/)                {$glob = "/2010/7GeV";}


  elsif ($pwd =~ /2011\/27GeV/)               {$glob = "/2011/27GeV";}
  elsif ($pwd =~ /2018\/27GeV/)               {$PICOPATH = "/gpfs01/star/data*"; $glob = "/reco/production_27GeV_fixedTarget_2018/ReversedFullField/P19ie/2018";}
#  elsif ($pwd =~ /2018\/3p85GeV_fixedTarget/) {$glob = "/2018/3p85GeV_fixedTarget";}
  elsif ($pwd =~ /2018\/3p85GeV_fixedTarget/) {$PICOPATH = "/gpfs01/star/data*";$glob = "/reco/production_3p85GeV_fixedTarget_2018/ReversedFullField/P19ie.SL20d/2018";}

  elsif ($pwd =~ /2019\/14GeV/)               {$glob = "/2019/14GeV_2019_TFG19e";}
  elsif ($pwd =~ /2019\/19GeV/)               {$glob = "/2019/19GeV_2019_TFG19e";}
  elsif ($pwd =~ /2019\/31GeV_fixedTarget/)   {$glob = "/2019/31GeV_fixedTarget_2019_TFG19l.Minuit";}
  elsif ($pwd =~ /2019\/26p2GeV_fixedTarget/)   {$glob = "/2019/26p2GeV_fixedTarget_2019_TFG19l.Minuit";}
  elsif ($pwd =~ /2019\/4p59GeV_fixedTarget/) {$glob = "/2019/4p59GeV_2019_TFG19e";}
  elsif ($pwd =~ /2019\/3p85GeV_fixedTarget/) {$glob = "/2019/3p85GeV_Fixed_2019_TFG19e";}
  elsif ($pwd =~ /2019\/7\.3GeV_fixedTarget/) {$glob = "/2019/7.3GeV_Fixed_2019_TFG19e";}
  elsif ($pwd =~ /2019\/7p7GeV/)              {$glob = "/2019/7p7GeV_2019_TFG19e";}
  elsif ($pwd =~ /2019\/9p2GeV/)              {$glob = "/2019/9p2GeV_2019_TFG19e";}
  elsif ($pwd =~ /2019\/AuAu200/)             {$glob = "/2019/AuAu200";}
#  TFG19m/RF/11p5GeV.B/            344-365 aka 11p5GeV
#  TFG19m/RF/11p5GeV.B/            001-027 
#  TFG19m/RF/5p75GeV_fixedTarget.B/355
#  TFG20a/RF/31p2GeV_fixedTarget/  028-029
#  TFG20a/RF/9p8GeV_fixedTarget/   029
#  TFG20a/RF/9p2GeV/               030
#  TFG20a/RF/9p8GeV_fixedTarget/   030-031
#  TFG20a/RF/19p5GeV_fixedTarget/  032
#  TFG20a/RF/9p8GeV_fixedTarget/   032
#  TFG20a/RF/13p5GeV_fixedTarget/  033
#  TFG20a/RF/19p5GeV_fixedTarget/  033
#  TFG20a/RF/13p5GeV_fixedTarget/  034
#  TFG20a/RF/9p2GeV/               034
#  TFG20a/RF/7p3GeV_fixedTarget/   035-036
#  TFG20a/RF/9p2GeV/               036-040
#  TFG20a/RF/9p2GeV/               041
#  TFG20a/RF/11p5GeV/              041-055 aka 11p5GeV.C
#  TFG20a/RF/5p75GeV_fixedTarget/  044
#  TFG20a/RF/5p75GeV_fixedTarget/  045
#  TFG20a/RF/9p2GeVb/              055-080
#  TFG20a/RF/9p2GeVc/              169-218
#  TFG20a/RF/26p5GeV_fixedTarget/  211
  elsif ($pwd =~ /2020\/5p75GeV_fixedTargetC/){$glob = "/2020/TFG20a/RF/5p75GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/5p75GeV_fixedTarget/) {$glob = "/2020/TFG19m/RF/5p75GeV_fixedTarget.B";}
  elsif ($pwd =~ /2020\/11p5GeV.C/)           {$glob = "/2020/TFG20a/RF/11p5GeV";}
  elsif ($pwd =~ /2020\/11p5GeV/)             {$glob = "/2020/TFG19m/RF/11p5GeV.B";}
  elsif ($pwd =~ /2020\/31p2GeV_fixedTarget/) {$glob = "/2020/TFG20a/RF/31p2GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/9p8GeV_fixedTarget/)  {$glob = "/2020/TFG20a/RF/9p8GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/9p2GeVb/)             {$glob = "/2020/TFG20a/RF/9p2GeVb";}
  elsif ($pwd =~ /2020\/9p2GeVc/)             {$glob = "/2020/TFG20a/RF/9p2GeVc";
					       print "pwd = $pwd +> glob = $glob; PICOPATH = $PICOPATH\n" if ($debug);
					     }
  elsif ($pwd =~ /2020\/9p2GeV/)              {$glob = "/2020/TFG20a/RF/9p2GeV";}
  elsif ($pwd =~ /2020\/13p5GeV_fixedTarget/) {$glob = "/2020/TFG20a/RF/13p5GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/19p5GeV_fixedTarget/) {$glob = "/2020/TFG20a/RF/19p5GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/26p5GeV_fixedTarget/) {$glob = "/2020/TFG20a/RF/26p5GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/7p3GeV_fixedTarget/)  {$glob = "/2020/TFG20a/RF/7p3GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/9p2GeV/)              {$glob = "/2020/TFG20a/RF/9p2GeV";}
  elsif ($pwd =~ /2020\/7p7GeV/)              {$glob = "/2020/TFG20a/RF/7p7GeV";}
  
}
print "PICOPATH = $PICOPATH; days = $dayMin  - $dayMax : glob = $glob\n" if ($debug);
if (! $glob) {die "glob = $glob";}
if (! $PICOPATH) {die "PICOPATH = $PICOPATH";}
#if ($glob == "" or $PICOPATH == "") {die "glob = $glob, PICOPATH = $PICOPATH";}
my $GLOB = $PICOPATH . $glob . "/???/*";
print "PICOPATH = $PICOPATH; days = $dayMin  - $dayMax : GLOB = $GLOB\n" if ($debug);
my %Runs= ();
foreach my $run (glob $GLOB) {
  my $f = File::Basename::basename($run);
  my $day = int ($f/1000);       # print "day = $day\n";
  my $Y = int ($day/1000);    # print "Y = $Y\n";
  $day -=  1000*$Y;           # print "day = $day\n";
  if ($dayMin > 0 && $day < $dayMin) {next;}
  if ($dayMax > 0 && $day > $dayMax) {next;}
  $Runs{$f}++;
  my $ana = $f . "_" . $Runs{$f} . ".root"; print "ana = $ana\n" if ($debug);
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
    print "string:$run:$ana:$year:picoDst\n";
    $Njobs++;
  } else {
    my $MuGlob = $run . "/*MuDst.root";
    my @Mus = glob $MuGlob;
    if ($#Mus > -1) {
      print "string:$run:$ana:$year:MuDst\n";
    }
  }
#  last;
#  die;
}
if (! $Njobs) {die "Don't have input files\n";}
