#! /usr/bin/env perl
use File::Basename;
use Cwd;
#my @particles = qw(pythia);
#my @particles = qw(pionMIP);
#my @particles = qw(electron muon pion kaon proton deuteron triton He3 alpha pionMIP);
#my @particles = qw(pion He3 alpha);
#my @particles = qw(electron positron);
my @particles = qw(muon+ muon- electron positron pion+ pion- kaon+ kaon- proton  pbar    deuteron triton He3 alpha pionMIP);
my @hist      = qw(muP   muN   eN       eP       piP   piN   kaonP kaonN protonP protonN deuteron triton He3 alpha piP);
for (my $i = 0; $i < 15; $i++) {
  my $glob = "*" . $hist[$i] . "*" . $particles[$i] . "*.root";
  my @files = glob $glob; #print "glob = $ glob : files = @files\n";
  print "@files\n";
}
