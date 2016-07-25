#! /usr/bin/env perl
use File::Basename;
use Cwd;
my @list = glob "/star/subsys/tpc/fisyak/reco/2014/1454/*.event.root";
foreach my $f (@list) {
  my $b = File::Basename::basename($f,".event.root");
  my $file = $b. ".tree.root";
  if (-r $file) {next;}
  print "string:$f\n";
}
