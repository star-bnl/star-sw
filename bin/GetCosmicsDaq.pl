#! /usr/bin/env perl
use File::Basename;
use Cwd;
 
#get_file_list.pl -keys 'path,filename,events' -cond 'filetype=online_daq,runnumber[]12032056-12180055,trgsetupname=CosmicLocalClock,storage=HPSS' -limit 0
# get_file_list.pl -delim "#" -all -keys 'magscale,path,basename,events' -cond 'filetype=online_daq,trgsetupname=CosmicLocalClock' -limit 0 | tee daq_full.list
#get_no_events.csh st_tofcosmic_12179074_raw_1010001.daq 
#get_file_list.pl -keys 'events' -cond 'filename='$1 -limit 1
my $pwd = cwd();
my $Field = "Undefined";
my $dir = File::Basename::basename($pwd);# print "dir = $dir\n";
if    ($dir eq 'RFF') {$Field = 'ReversedFullField';}
elsif ($dir eq 'FF' ) {$Field = 'FullField';}
elsif ($dir eq 'ZF' ) {$Field = 'FieldOff';}
if ($Field eq "Undefined") {die "Field is not defined";}
my $tmp = "tmp.list";
if (-r "tmp.list") {unlink $tmp;} # magscale, events
my $cmd = "get_file_list.pl -delim \"#\" -keys 'path,basename,events' -cond 'magscale=$Field,filetype=online_daq,trgsetupname=CosmicLocalClock,events>1000' -limit 0 >> $tmp";
#print "cmd = $cmd\n";
my @list = system($cmd);
open(In, $tmp) or die "Can't open $tmp";
my $line;
while ($line = <In>) {
  chop($line);
#  print "$line";
  my ($path,$file,$events) = split /\#/, $line;
  if ($path !~ /\/2011\//) {next;}
  if ($file =~ /adc/ || $file =~ /laser/) {next;}
  my $fileN = $file;
  $fileN .= "_1_" . $events;
  my $root_file = $fileN . ".event.root";
  my $mudst_file = $fileN . ".MuDst.root";
  if (-r $root_file && -r $mudst_file) {next;}
  $path =~ s#/home/starsink/raw#/star/data03#;
  my $fullpath = $path . "/" . $file . ".daq";
  if (! -r $fullpath) {next;}
  print "string:$fullpath:1:$events\n";
}
