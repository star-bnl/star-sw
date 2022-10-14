#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my $reduction = 0;
my $pwd = cwd();
my $debug = 0;
if ($#ARGV < 0) {
  print "Usage: cd /hlt/cephfs/daq/2022; $0 2\n";
  exit;
} else {
  $reduction = $ARGV[0];
}
if ($reduction <= 1 || $reduction > 100) {die "Illegal reduction factor = $reduction";}
#my @runlist = glob "22*"; #print "\# $#runlist = @runlist\n";
my @runlist = glob "*/*"; print "\# $#runlist = @runlist\n" if ($debug);
my $n = 0;
foreach my $run (@runlist) {
  $n++;
  $i = $n%$reduction;
  if ($i == 1) {
    print "#keep $run\n";
      next;
  }
   print "rm -rf $run\n";
}

