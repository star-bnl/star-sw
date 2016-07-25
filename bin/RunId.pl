#! /usr/bin/env perl
use File::Basename;
use Cwd;
if ($#ARGV < 0) {
  print "Usage: $0 no_of_runs\n";
  exit 0;
}
my $NF = $ARGV[0];
my $f1 = 1;
if ($#ARGV >= 1) {
  $f1 = $ARGV[0];
  $NF = $ARGV[1];
}
for (my $f = $f1; $f <= $NF; $f++) {
    print "string:$f:1:100\n";
}
