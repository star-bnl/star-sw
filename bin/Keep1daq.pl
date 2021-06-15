#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my @Runs = glob "*/*";
my $n = 0;
foreach my $run  (@Runs) {
  my @daqs = glob $run  . "/hlt*.daq";
#  print "$run => @daqs\n";
  if ($#daqs <= 0) {next;}
  my $n = 0;
  foreach my $file (@daqs) {
    $n++;
    if ($n != 2) {print "rm    $file\n"}
    else         {print "\#keep $file\n"}
  }
}
