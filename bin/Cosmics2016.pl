#! /usr/bin/env perl
use File::Basename;
use Cwd;
 
#get_file_list.pl -keys 'path,filename,events' -cond 'filetype=online_daq,runnumber[]12032056-12180055,trgsetupname=CosmicLocalClock,storage=HPSS' -limit 0
# get_file_list.pl -delim "#" -all -keys 'magscale,path,basename,events' -cond 'filetype=online_daq,trgsetupname=CosmicLocalClock' -limit 0 | tee daq_full.list
#get_no_events.csh st_tofcosmic_12179074_raw_1010001.daq 
#get_file_list.pl -keys 'events' -cond 'filename='$1 -limit 1
#my $pwd = cwd();
#my $Field = "Undefined";
#my $dir = File::Basename::basename($pwd);# print "dir = $dir\n";
#if    ($dir eq 'RFF') {$Field = 'ReversedFullField';}
#elsif ($dir eq 'FF' ) {$Field = 'FullField';}
#elsif ($dir eq 'ZF' ) {$Field = 'FieldOff';}
#if ($Field eq "Undefined") {die "Field is not defined";}
#my $tmp = "tmp.list";
#if (-r "tmp.list") {unlink $tmp;} # magscale, events
#my $cmd = "get_file_list.pl -delim \"#\" -keys 'path,basename,events' -cond 'magscale=$Field,filetype=online_daq,trgsetupname=CosmicLocalClock,events>10000' -limit 0 >> $tmp";
#print "cmd = $cmd\n";
#my @list = system($cmd);
#my @list = glob "/star/institutions/bnl/fisyak/Tpc/Alignment/Y2011RC.L/$dir/*.event.root";
#my @list = glob "/star/data03/daq/2014/*/*/st_cosmic_adc_15*.daq";
my $pwd = Cwd::cwd();# print "pwd = $pwd\n";
#my @list = glob "/star/data03/daq/2014/*/*/st_cosmic_adc_15*.daq /gpfs01/star/scratch_delete/fisyak/daq/2014/*/*/st_cosmic_adc_15*.daq";
my @list = glob "/star/data03/daq/2016/???/*/st_cosmic*.daq /star/data03/daq/2016/???/*/st_gmt*.daq";
#my $FFirstRun  = 16021045;
#my $FFLastRun  = 16029052;
#my $RFirstRun  = 16031005;
#my $RFLastRun  = 16173033;
#if ($pwd =~ /FF/) {@list = glob "/star/data03/daq/2016/021/*/st_cosmic*.daq";}
#else              {@list = glob "/star/data03/daq/2016/031/*/st_cosmic*.daq";} # RF
foreach my $fullpath (@list) {
  my $file = $fullpath;
  $file =~ s#/star/data03/daq/2016/##;
#  $file =~ s#/gpfs01/star/scratch_delete/fisyak/daq/2014/##;
  my ($dummy,$run) = split('/',$file); #print "$file => $run\n";
#  if (! (($run >= $FFirstRun && $run <= $FFLastRun) or 
#         ($run >= $RFirstRun && $run <= $RFLastRun))) {next;} 
  my $base = File::Basename::basename($fullpath,".daq");
#  print "$fullpath => $base\n";
#  if ($rootfile =~ m/$BadFiles/) {next;}
  my $rootfile = $base . ".event.root";
  if (-r $rootfile) {next;}
  my $mufile   = $base . ".MuDst.root";
  if (-r $mufile) {next;}
  my $logfile  = $base . "B.log";
  if (-r $logfile) {next;}
  print "string:$fullpath\n";
#  last;
}
