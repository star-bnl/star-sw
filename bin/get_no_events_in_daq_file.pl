#! /usr/bin/env perl
use File::Basename;
use Cwd;
if ($#ARGV != 0) {
  print "Usage $0 st_cosmic_14046106_raw_1040002.daq\n";
  exit 0;
}
my $cmd = "get_file_list.pl -keys 'events' -cond 'filetype=online_daq,filename=" . $ARGV[0] . "'";
print "events = ";
my $flag = system($cmd);
#print "$flag events\n";
