#! /usr/bin/env perl
#print "$#ARGV\n";
if ($#ARGV < 0) {
  print "Usage : $0 day\n";
  exit 0;
}
my $debug = 1;
my $day = $ARGV[0];
my $trigger = "AuAu_200_production_mid_2014";
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename,events' -cond 'filetype=online_daq,filename~st_laser,tpx=1,sanity=1,runnumber>15000000,events>10' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_15,tpx=1,sanity=1,runnumber>15000000,events>10' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_,tpx=1,sanity=1,events>10,runnumber>16140032,trgsetupname=fixedTarget2015' -limit 0`; 
#get_file_list.pl -keys path,filename -cond production=P16ij,trgsetupname=AuAu_200_production_2016,filetype=daq_reco_MuDst,filename~st_physics,storage=nfs -limit 0
#my  @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'production=P16ij,trgsetupname=AuAu_200_production_2016,filetype=daq_reco_MuDst,filename~st_physics,tpx=1,pxl=1,ist=1,sanity=1,runnumber>17124000,events>10' -limit 0`;
#my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'production=P16ij,trgsetupname=AuAu_200_production_2016,filetype=daq_reco_MuDst,tpx=1,pxl=1,ist=1,sanity=1,events>10,runnumber>17000000' -limit 0"; print "$cmd\n" if ($debug);
my $TriggerFile = $trigger . ".txt"; print "Trigger file = $TriggerFile\n" if ($debug);
if (! -r $TriggerFile) {
#  my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'production=P16id,trgsetupname=" . $trigger . ",filetype=daq_reco_MuDst,filename~st_physics,tpx=1,pxl=1,ist=1,sanity=1,events>10,runnumber>15000000' -limit 0"; 
  my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'production=P16id,trgsetupname=" . $trigger . ",filetype=daq_reco_MuDst,filename~st_physics_adc,tpx=1,pxl=1,ist=1,sanity=1,events>10,daynumber=166' -limit 0"; 
  print "$cmd\n" if ($debug);
  my  @list = `$cmd`;
  open(Out, ">$TriggerFile") or die "Can't open $TriggerFile"; 
  foreach my $file (@list) {
    print Out "$file";
  }
  close (Out);
}
#my @HLT = qw(17125034 17127006 17127007 17127008 17127009 17127010 17127011 17127012 17127013 17127014
#	     17127016 17127017 17127026 17128002 17128003 17128006 17128007 17128008 17128009 17128010);
#my $HLT = join '|', @HLT;
my $i = 1;   
my $OldRun = 0;
open(In, "$TriggerFile") or die "Can't open $TriggerFile";
while (my $file = <In>) { 
  chomp($file);
  my @words = split('/',$file);
  my $year = int $words[7];
  my $dayr = int $words[8]; 
  if ($dayr ne $day) {next;}
  my $run =  int $words[9]; 
#  if ($run =~ $HLT) {next;}
#  my $year = int $run/1000000;
#  my $day = int $run/1000 - 1000 * $year;
  print "run = $run year = $year day = $day \n" if ($debug);
#  die;
#  print "run = $run\n";
  if ($run != $OldRun) {$i = 1; $OldRun = $run;}
  else                 {$i++;}
#  if ($i%20 != 1) {next;}
  my $basename = $file;
  $basename =~ s#home/starreco/reco/AuAu_200_production_2016/ReversedFullField/P16ij/##;
  $basename =~ s#home/starreco/reco/AuAu_200_production_mid_2014/ReversedFullField/P16id/##;
#  print "$file = > $basename\n";
  my $found = 0;
  my $dir;
  my @dirs = qw( /gpfs02/eic/ayk/STAR/reco/MuDst /gpfs01/star/pwg/fisyak/MuDst );
  foreach $dir (@dirs) {
    my $fullpath = $dir . $basename;
    print "$i : $fullpath ===========" if ($debug);
    if (-f $fullpath) {
      $found = 1; 
      print "found\n" if ($debug);
      last;
    }
    print "not found\n" if ($debug);
  }
  if (! $found) {
    my $piconame = $basename;
    $piconame =~ s/MuDst/picoDst/;
    my $pico = "/gpfs01/star/pwg_tasks/hf03/Pico" . $piconame; print "$pico\n" if ($debug);
    if (-r $pico) {
      print "$pico is Done\n" if ($debug);
      next;
    }
    print "$file $dirs[0]$basename\n";
  }
}
close (In);
