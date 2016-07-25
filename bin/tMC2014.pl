#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#my @list = glob "/star/data03/daq/2013/15[4-9]/*/st_physics*.daq";
#my @list = glob "/gpfs01/star/scratch_delete/fisyak/daq/2014/100/*/st_physics*.daq";
my @list = glob "/star/subsys/tpc/fisyak/simu/2014/AuAu200Z6cmBL3/*.fz";# /gpfs01/star/scratch_delete/fisyak/daq/2014/100/*/st_physics*.daq ";
#my $MaxEvents = 500; #
my $MaxEvents = 5000;
my $step      = 5000;
my $firstevent = 1;
foreach my $line (@list) {
#  chop($line);
  my $file = File::Basename::basename($line,".fz");
  my ($dummy,$dummy,$run) = split('_',$file); # print "$run\n";
  my $rootf = $file . ".MuDst.root";
  if (-r $rootf) {next;}
#  for (my $events = $firstevent; $events < $MaxEvents; $events += $step) {
#    my $lastevent = $events + $step - 1;
#    print "string:$line:$events:$lastevent\n";    
  print "string:$line\n";
#  last;
#  }
#  last;
}
