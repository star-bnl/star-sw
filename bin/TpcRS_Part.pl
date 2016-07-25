#! /usr/bin/env perl
use File::Basename;
use Cwd;
#my @particles = qw(pythia);
#my @particles = qw(pionMIP);
#my @particles = qw(electron muon pion kaon proton deuteron triton He3 alpha pionMIP);
#my @particles = qw(pion He3 alpha);
#my @particles = qw(electron positron);
#my @particles = qw(muon+ muon- electron positron pion+ pion- kaon+ kaon- proton pbar deuteron triton He3 alpha pionMIP);
my @particles = qw(muon+ muon- electron positron pion+ pion- kaon+ kaon- proton pbar deuteron triton He3 alpha);
#my @particles = qw(proton pionMIP);
#my @particles = qw(pion+ pion-);
#my @particles = (pionMIP);
##my @particles = (pionMIP1);
#my @particles = (phi);
foreach my $part (@particles) {
  my $i1 = 1;
  my $N =  10;# 00;
  if ($part =~ /MIP/) {$N = 100;}
  for (my $i = $i1; $i <= $N; $i++) {
    my $log = $part . ":" . $i . "B.log";
    if (! -r $log) {
      print "string:$part:$i\n";
    }
#    last;
  }
#  last;
}
my @cmd = qw(
foreach f (muon tron pion proton pbar deuteron triton He3 alpha pionMIP)
	    echo ${f}; hadd -T ${f}.root gstar*${f}*0.root
end
);
