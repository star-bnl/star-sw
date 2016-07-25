#!/usr/bin/env perl
#  stripChainFromLogFile.pl /star/rcf/test/dev/trs_sl302.ittf/Mon/*/*/*.log | tee sim.chain
use File::Basename;
use FileHandle;
if ($#ARGV < 0) {
  print "Usage $0 log_files\n";
  exit 0;
}
foreach my $log (@ARGV) {
  open (In,$log) or die "Can't open $log";
  print "$log\n";
  my  $gopt = 0;
  while (my $line = <In>) {
    if ($gopt) {
      if ($line =~ /^QA :INFO  - Run/ ||
	  $line =~ /^QAInfo: doPs for/) {last;}
      print $line;
    } elsif ($line =~ /^QA :INFO  - \+\+\+ Setting attribute/) {$gopt = 1;}
    else {next;}
  }
  close(In);
}

