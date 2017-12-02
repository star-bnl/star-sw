#! /usr/bin/env perl
#print "$#ARGV\n";
if ($#ARGV < 0) {
  print "Usage : $0 day\n";
  exit 0;
}
my $debug = 0;
my $day = $ARGV[0];
my $trigger = "AuAu7_production";
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename,events' -cond 'filetype=online_daq,filename~st_laser,tpx=1,sanity=1,runnumber>15000000,events>10' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_15,tpx=1,sanity=1,runnumber>15000000,events>10' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_,tpx=1,sanity=1,events>10,runnumber>16140032,trgsetupname=fixedTarget2015' -limit 0`; 
#get_file_list.pl -keys path,filename -cond production=P16ij,trgsetupname=AuAu_200_production_2016,filetype=daq_reco_MuDst,filename~st_physics,storage=nfs -limit 0
#my  @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'production=P16ij,trgsetupname=AuAu_200_production_2016,filetype=daq_reco_MuDst,filename~st_physics,tpx=1,pxl=1,ist=1,sanity=1,runnumber>17124000,events>10' -limit 0`;
#my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'production=P16ij,trgsetupname=AuAu_200_production_2016,filetype=daq_reco_MuDst,tpx=1,pxl=1,ist=1,sanity=1,events>10,runnumber>17000000' -limit 0"; print "$cmd\n" if ($debug);
my $TriggerFile = $trigger . ".list"; print "Trigger file = $TriggerFile\n" if ($debug);
if (! -r $TriggerFile) {
#  my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'production=P10ih,trgsetupname=" . $trigger . ",filetype=daq_reco_MuDst,filename~st_physics,tpx=1,sanity=1,events>10' -limit 0"; 
  my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'production=P10ih,trgsetupname=" . $trigger . ",filetype=daq_reco_MuDst,filename~st_physics,sanity=1,tpx=1' -limit 0";
  print "$cmd\n" if ($debug);
  my  @list = `$cmd`;
  open(Out, ">$TrigerFile") or die "Can't open $TriggerFile";
  foreach my $file (@list) {
    print Out "$file";
  }
  close (Out);
}
my $i = 1;   
my $OldRun = 0;
open(In, "$TriggerFile") or die "Can't open $TriggerFile";
while (my $file = <In>) { 
  chomp($file);
  my @words = split('/',$file);
  my $year = int $words[7];
  my $dayr = int $words[8]; 
#  if ($dayr ne $day) {next;}
  my $run =  int $words[9]; 
#  my $year = int $run/1000000;
#  my $day = int $run/1000 - 1000 * $year;
  print "run = $run year = $year day = $day \n" if ($debug);
#  die;
#  print "run = $run\n";
  if ($run != $OldRun) {$i = 1; $OldRun = $run;}
  else                 {$i++;}
#  if ($i%20 != 1) {next;}
  my $basename = $file;
  $basename =~ s#home/starreco/reco/AuAu7_production/ReversedFullField/P10ih/##;
  $basename =~ s#home/starlib##;
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
