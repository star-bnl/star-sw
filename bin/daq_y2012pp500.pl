#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my @list = glob "/star/data03/daq/2012/108/13108073/st_physics_13108073*.daq";
#my $MaxEvents = 500; #
my $MaxEvents = 5000;
my $step      = 200;
my $firstevent = 301;
foreach my $line (@list) {
#  chop($line);
  for (my $events = $firstevent; $events < $MaxEvents; $events += $step) {
    my $lastevent = $events + $step - 1;
    print "string:$line:$events:$lastevent\n";
  }
#  last;
}
