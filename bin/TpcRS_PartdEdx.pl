#! /usr/bin/env perl
use File::Basename;
use Cwd;
my @particles = qw(electron muon pion kaon proton deuteron triton He3 alpha);# pionMIP);
#my @particles = qw(electron He3);# alpha);
#my @particles = qw(electron positron);
foreach my $part (@particles) {
  my $i1 = 1;
  my $N = 1;
#  my $N = 10;
#  if ($part eq 'pionMIP') {$N = 100;}
  for (my $i = $i1; $i <= $N; $i++) {
    print "string:$part:$i\n";
  }
}
