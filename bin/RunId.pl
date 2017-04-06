#! /usr/bin/env perl
use File::Basename;
use Cwd;
if ($#ARGV < 0) {
  print "Usage: $0 no_of_runs\n";
  exit 0;
}
my $NR = $ARGV[0];
my $r1 = 1;
if ($#ARGV >= 1) {
  $r1 = $ARGV[0];
  $NR = $ARGV[1];
}
my $nevents = 100;
if ($#ARGV >= 2) {
  $nevents = $ARGV[2];
}
my $name = "Lc3pi";
if ($#ARGV >= 3) {
  $name = $ARGV[3];
}

for (my $r = $r1; $r <= $NR; $r++) {
    print "string:$r:1:$nevents:$name\n";
}
