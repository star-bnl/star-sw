#! /usr/bin/env perl
use File::Basename;
use Cwd;
#my @particles = qw(pythia);
#my @particles = qw(pionMIP);
#my @particles = qw(electron muon pion kaon proton deuteron triton He3 alpha pionMIP);
#my @particles = qw(pion He3 alpha);
#my @particles = qw(electron positron);
#my @particles = qw(muon+ muon- electron positron pion+ pion- kaon+ kaon- proton pbar deuteron triton He3 alpha); # pionMIP);
#my @particles = qw(pion+ pion-);
my @particles = (hijing);
foreach my $part (@particles) {
  my $i1 =201;
  my $N =1000;
  for (my $i = $i1; $i <= $N; $i++) {
    my $log = $part . ":" . $i . "B.log";
    if (! -r $log) {
      print "string:$part:$i\n";
    }
#    last;
  }
#  last;
}
