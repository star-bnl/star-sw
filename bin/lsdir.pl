#! /usr/bin/env perl
use File::Basename;
my $glob = "/gpfs02/eic/ayk/STAR/reco/MuDst/2010/*/11*";
foreach my $file (glob $glob) {
  my $f = File::Basename::basename($file);
  my $pico = $f . ".picoDst.root";
  if ( -r $pico) {next};
  print "string:$file\n";
#  last;
}
