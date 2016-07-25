#!/usr/bin/env perl
use File::Basename;
if ($#ARGV < 0) {
  print "Usage $0 list_of_files\n";
  exit 0;
}
open(In, $ARGV[0]) or die "Cannot open $ARGV[0]";
my $line;
my $oldRun = -1;
while ($line = <In>) {
  if ($line =~ /adc/) {next;}
  chop($line);
  my @words = split '/', $line;
  my $run = $words[7]; #print "$line => $run \n";
  if ($run == $oldRun) {next;}
  $oldRun = $run;
  $line =~ s/\.daq.*/\.daq/;
  my $bfile = File::Basename::basename($line);
#  print "$line\n";
  print "$bfile\n";
}
close(In);
