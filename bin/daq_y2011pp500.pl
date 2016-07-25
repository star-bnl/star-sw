#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my @list = glob "/star/data03/daq/2011/038/12038078/st_physics_*.daq";
#my $MaxEvents = 500; #
my $MaxEvents = 5000;
my $step      = 200;
my $firstevent = 1;
foreach my $line (@list) {
#  chop($line);
  for (my $events = $firstevent; $events < $MaxEvents; $events += $step) {
    my $lastevent = $events + $step - 1;
    print "string:$line:$events:$lastevent\n";
  }
#  last;
}
