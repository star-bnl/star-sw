#! /usr/bin/env perl
use File::Basename;
use Cwd;

#my $vers = "B"; # default geometry
#my $vers = "G"; # add walls           <==========
#my $vers = "U.1ks", # increase time cut to 1e3 sec <========
#my $vers = "W"; # enlarged tcut
#my $vers = "V"; # new concrete and new counter postion
#my $vers = "U"; # extern histograms
#my $vers = "Za"; # add PMMA and BEMC electronics
#my $vers = "Zb"; # use TRandom3
#my $vers = "Zc"; # use TRandom3(0 ) <=========
#my $vers = "Zd"; # remove PMMA from MAGP
#my $vers = "Ze"; # add PMMA to MAGP, old definition of concrete
my $ev1 = 1;
my $ev2 = 1000;
#my $ev2 = 1;
my @names = qw(K0s gamma Lambda AntiLambda);
foreach my $name (@names) {
  for (my $r = 1; $r <= 10 ; $r++) {
    my $rfile = $name . "_" . $r . ".MuDst.root";# print "$rfile\n";
    if (-r $rfile) {next;}
    print "string:$r:1000:$name\n"; #:$ev1:$ev2\n";
  }
#  last;
}
