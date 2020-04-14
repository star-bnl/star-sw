#!/usr/bin/env perl
my $def = "Freq.log";
open (In, $def) or die "Can't open $def";
my $line;
my $OldFreq = 0;
while ($line = <In>) {
  my ($dummy,$dummy,$run) = split'_',$line;
  my ($dummy,$dummy,$Freq) = split ' ',$line;
  if ($Freq ne $OldFreq and $Freq ne "9.4e+06") {
    print "run = $run, Freq = $Freq\n";
    $OldFreq = $Freq;
  }
}
close(In);
