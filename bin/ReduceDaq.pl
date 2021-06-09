#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my $reduction = 0;
my $pwd = cwd();
if ($#ARGV < 0) {
  print "Usage: cd /hlt/cephfs/daq/2021/142; $0 4\n";
  exit;
} else {
  $reduction = $ARGV[0];
}
if ($reduction <= 1 || $reduction > 4) {die "Illegal reduction factor = $reduction";}
my @List = glob "*/hlt*.daq";
if ($#List < 100) {die "Only $#List hlt*daq files found in #pwd"; exit;}
print "# Reduce no. $#List files by a factor of $reduction\n";
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
