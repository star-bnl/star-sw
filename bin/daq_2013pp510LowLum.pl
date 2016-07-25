#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#my @list = glob "/star/data03/daq/2013/15[4-9]/*/st_physics*.daq";
my @list = glob "/star/data03/daq/2013/157/*/st_physics*.daq";
#my $MaxEvents = 500; #
my $MaxEvents = 5000;
my $step      = 5000;
my $firstevent = 1;
foreach my $line (@list) {
#  chop($line);
  for (my $events = $firstevent; $events < $MaxEvents; $events += $step) {
    my $lastevent = $events + $step - 1;
    print "string:$line:$events:$lastevent\n";
  }
#  last;
}
