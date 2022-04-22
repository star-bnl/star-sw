#! /usr/bin/env perl
use File::Basename;
use Cwd;
 
#get_file_list.pl -keys 'path,filename,events' -cond 'filetype=online_daq,runnumber[]12032056-12180055,trgsetupname=CosmicLocalClock,storage=HPSS' -limit 0
# get_file_list.pl -delim "#" -all -keys 'magscale,path,basename,events' -cond 'filetype=online_daq,trgsetupname=CosmicLocalClock' -limit 0 | tee daq_full.list
#get_no_events.csh st_tofcosmic_12179074_raw_1010001.daq 
#get_file_list.pl -keys 'events' -cond 'filename='$1 -limit 1
my $pwd = cwd(); # print "pwd = $pwd\n";
my $Field = "Undefined";
my $dir = File::Basename::basename($pwd);# print "dir = $dir\n";
my $GLOB = "";
if ($pwd =~ /2022/) {
  $GLOB = "/hlt/cephfs/reco/2022";
} else {
  die "No run defied";
}
if ($pwd =~ /FF/) {
  $GLOB .= "/FF/Cosmic/*/*/*event.root";
} elsif ($pwd =~ /RF/) {
  $GLOB .= "/RF/Cosmic/*/*/*event.root";
} else {
  die "Field has not defined";
}
#print "GLOB = $GLOB\n";
my @list = glob $GLOB;
foreach my $fullpath (@list) {
#  my $file = File::Basename::basename($fullpath,".event.root");
#  print "$fullpath => $file\n";
#  if ($file =~ m/$BadFiles/) {next;}
  print "string:$fullpath\n";
}
