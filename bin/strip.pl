#!/bin/env perl
my $line = "";
my $dateOld = "";
my $timeOld = "";
my $deadAliveOld = "";
while ($line = <>) {
  my ($date,$time,$name,$deadAlive) = split ' ',$line;
#  print "$date|$time|$name|$deadAlive\n";
  if ($date eq $dateOld and $time eq $timeOld and $deadAliveOld eq 'alive' and $deadAlive = 'dead') {
#    print "skip\n";
  }
  else {
    $dateOld = $date;
    $timeOld = $time;
    $deadAliveOld = $deadAlive;
    print $line;
  }
}
