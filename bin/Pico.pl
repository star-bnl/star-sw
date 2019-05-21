#! /usr/bin/env perl
use File::Basename;
#my $glob =  "/net/l404/data/fisyak/Pico/BES-I/AuAu19_production/2011/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/TFG19d/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/14GeV_2019_StiCA/0??/*";
 my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/14GeV_2019_TFG19e/???/20*";
#y $glob = "/gpfs01/star/data*/reco/production_14p5GeV_2019/ReversedFullField/dev/2019/*/*";
foreach my $file (glob $glob) {
  my $f = File::Basename::basename($file);
  my @words = split('/',$file);# print "disk = $words[3]\n";
#  my $pico = $f . $words[3] . ".root"; print "$pico\n";
  my $pico = $f  . ".root";# print "$pico\n";
  if ( -r $pico) {next};
  print "string:$file\n";
#  last;
}
