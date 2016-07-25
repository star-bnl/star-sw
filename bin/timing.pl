#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $file = "/afs/rhic.bnl.gov/star/users/fisyak/bin/timing.list";
open(In, $file) or die "Can't open $file";
while ( my $it = <In>) {
  my @words = split(":",$it);
  my $log = $words[0] . "_" . $words[1] . "_" . $words[2] . "_" . $words[3] . "/st_physics_11029020_raw_1030002.log";
  if (-r $log) {next;}
  print "string:$it";
}

