#! /usr/bin/env perl
use File::Basename;
#my $glob =  "/net/l404/data/fisyak/Pico/BES-I/AuAu19_production/2011/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/TFG19d/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/14GeV_2019_StiCA/0??/*";
my $glob =  "/gpfs01/star/data*/reco/production_11p5GeV_2020/*/*/*";
my @count = 0;
foreach my $dir (glob $glob) {
  $count++;
  my $cmd = "ln -sf $dir Pico" . $count;
  print "cmd = $cmd\n";
  my $flag = system($cmd);
#  last;
}
