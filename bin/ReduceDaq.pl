#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my $reduction = 0;
my $pwd = cwd();
my $debug = 0;
if ($#ARGV < 0) {
  print "Usage: cd /hlt/cephfs/daq/2021/142; $0 20\n";
  exit;
} else {
  $reduction = $ARGV[0];
}
if ($reduction <= 1 || $reduction > 100) {die "Illegal reduction factor = $reduction";}
#my @runlist = glob "22*"; #print "\# $#runlist = @runlist\n";
my @runlist = glob "2*"; print "\# $#runlist = @runlist\n" if ($debug);
foreach my $run (@runlist) {
  my @List = glob  $run . "/hlt*.daq"; print "\# $#List = @List\n" if ($debug);
  if ($#List < 2) {print "\# Only $#List hlt*daq files found for $run\n"; next;}
  print "\# Reduce no. $#List files by a factor of $reduction\n";
  my $n = 0;
  foreach my $file (@List) {
    $n++;
    $i = $n%$reduction;
    if ($i == 1) {
      print "#keep $file\n";
      next;
    }
    print "rm $file\n";
  }
}
