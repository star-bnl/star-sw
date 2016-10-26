#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $N = 10;
if ($#ARGV >= 0) {$N = $ARGV[0];}
my $part = "Mickey";
for (my $i = 1; $i <= $N; $i++) {
  my $log = $part . ":" . $i . "B.log";
  if (! -r $log) {
    print "string:$part:$i\n";
  }
}

