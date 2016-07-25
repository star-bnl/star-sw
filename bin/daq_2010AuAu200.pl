#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#my @list = glob "/star/data03/daq/2010/15[4-9]/*/st_physics*.daq";
#my @list = glob "/gpfs01/star/scratch/fisyak/daq/2014/*/*/st_physics*.daq";
my @list = glob "/star/data03/daq/2010/*/*/st_hlt_1*.daq";
#my $MaxEvents = 500; #
my $MaxEvents = 5000;
my $step      = 5000;
my $firstevent = 1;
foreach my $line (@list) {
#  chop($line);
  my ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime, $mtime, $ctime, $blksize, $blocks ) = stat $line;
#  print "$line => $size\n";
  next if $size < 1000000000;
  my $file = File::Basename::basename($line,".daq");
    my $rootf = $file . ".event.root";
    if (-r $rootf) {next;}
#  for (my $events = $firstevent; $events < $MaxEvents; $events += $step) {
#    my $lastevent = $events + $step - 1;
#    print "string:$line:$events:$lastevent\n";    
  print "string:$line\n";
#  last;
#  }
#  last;
}
